(in-package #:sql-query-postgres/tests)

(deftest ddl-postgres-backend
  (let* ((d (make-postgres-dialect))
         (ct (create-table :users
                (column :id :type :integer :primary-key t :autoincrement t)
                (column :email :type '(:varchar 255) :unique t)))
         (sql (nth-value 0 (compile-sql ct :dialect d))))
    (ok (search "SERIAL PRIMARY KEY" sql))
    (ok (search "VARCHAR(255)" sql))))

(deftest postgres-params-dollar
  (let* ((d (make-postgres-dialect))
         (stmt (select (columns :id) (from :t)
                       (where (:= :id (bindparam :id 42)))))
         (sql (nth-value 0 (compile-sql stmt :dialect d)))
         (params (nth-value 1 (compile-sql stmt :dialect d))))
    (ok (search "$1" sql))
    (ng (search "?" sql))
    (ok (equal '(42) params)))
  (let* ((d (make-postgres-dialect))
         (sql (nth-value 0 (compile-sql
                            (select (columns :id) (from :t) (where (:= :id 42)))
                            :dialect d))))
    (ok (search "= 42" sql) "bare literal inlined")
    (ng (search "$" sql))))

(deftest procedure-postgres
  (let* ((d (make-postgres-dialect))
         (stmt (create-procedure :bump
                  (params (in :by :integer))
                  (body (sql-fragment "UPDATE counters SET n = n + ?" 1))))
         (sql (nth-value 0 (compile-sql stmt :dialect d))))
    (ok (search "CREATE PROCEDURE" sql))
    (ok (search "LANGUAGE plpgsql" sql))
    (ok (equal '(1) (nth-value 1 (compile-sql stmt :dialect d))))
    (ok (search "CALL"
                (nth-value 0 (compile-sql (sql-call :bump 1) :dialect d))))))

(deftest dialect-registry
  (ok (typep (gethash :postgres sql-query:*sql-dialect-registry*)
             'postgres-dialect)))

(deftest postgres-seeded-jsonb-and-ops
  (let ((d (make-postgres-dialect)))
    (ok (string= "JSONB" (dialect-type-sql d :jsonb)))
    (ok (string= "JSON" (dialect-type-sql d :json)))
    (ok (string= "INTEGER[]" (dialect-type-sql d '(:array :integer))))
    (multiple-value-bind (sql params)
        (compile-sql
         (select (columns (ensure-expr '(:->> :payload "name")))
                 (from :events)
                 (where (ensure-expr `(:@> :payload ,(typed "{\"ok\":true}" :jsonb)))))
         :dialect d)
      (%assert-contains sql "->>" "@>" "CAST(" "AS JSONB" "'name'" "'{\"ok\":true}'")
      (ok (null params)))))

(deftest postgres-types-datetime-json-array
  (let ((d (make-postgres-dialect)))
    (ok (string= "JSONB" (dialect-type-sql d :jsonb)))
    (ok (string= "TIMESTAMPTZ" (dialect-type-sql d :timestamptz)))
    (ok (string= "INTEGER[]" (dialect-type-sql d '(:array :integer))))
    (ok (string= "BYTEA" (dialect-type-sql d :bson)))
    (let ((sql (%sql (select (columns (typed 1700000000 :timestamptz)
                                      (typed '(1 2 3) :int-array)
                                      (typed "{\"a\":1}" :jsonb))
                             (from :t))
                     d)))
      (%assert-contains sql "to_timestamp" "ARRAY[" "CAST(" "AS JSONB"))))

(deftest postgres-ops-and-helpers
  (let* ((d (make-postgres-dialect))
         (sql (%sql (select
                     (columns (jsonb-text :payload "name")
                              (jsonb-ref :payload "meta")
                              (date-trunc "day" :created)
                              (now)
                              (sql-func :extract (sql-raw "hour") :created)
                              (sql-func :current-date)
                              (array-lit 1 2 3)
                              (array-agg :id))
                     (from :events)
                     (where (jsonb-contains
                             :payload (typed "{\"ok\":true}" :jsonb))))
                    d)))
    (%assert-contains sql
                      "->>" "->" "date_trunc" "now(" "EXTRACT(" "FROM"
                      "CURRENT_DATE" "ARRAY[" "array_agg" "@>" "AS JSONB")))

(deftest postgres-func-registry-rename
  (let ((d (make-postgres-dialect)))
    (ok (find-sql-func d :jsonb-set))
    (ok (string= "jsonb_set" (sql-func-sql-name (find-sql-func d :jsonb-set))))
    (%assert-contains (%sql (select (columns (sql-func :gen-random-uuid)) (from :t)) d)
                      "gen_random_uuid(")))

(deftest layer2-postgres-plpgsql
  (let* ((d (make-postgres-dialect))
         (stmt (create-procedure :bump
                  (params (in :by :integer) (inout :n :integer))
                  (body
                   (let ((tmp :integer 0))
                     (if (:= :n 0)
                         (setf :n :by)
                         (setf :n (:+ :n :by)))
                     (loop :while (:< :tmp 3)
                           :do (setf :tmp (:+ :tmp 1)))
                     (when (:> :n 1000) (return))))))
         (sql (nth-value 0 (compile-sql stmt :dialect d))))
    (%assert-contains sql
                      "LANGUAGE plpgsql" "AS $$" "DECLARE" ":="
                      "IF " "THEN" "ELSE" "END IF"
                      "WHILE " "LOOP " "END LOOP" "EXIT"
                      "INTEGER := 0")
    (%assert-absent sql "END WHILE" "LEAVE")
    (ng (search "$1" sql) "no bind placeholders ($$ is plpgsql quoting)")
    (ok (null (nth-value 1 (compile-sql stmt :dialect d))))))

(deftest create-type-enum-postgres
  (let* ((d (make-postgres-dialect))
         (sql (nth-value 0 (compile-sql
                            (create-type :mood :enum '("sad" "ok" "happy"))
                            :dialect d)))
         (alter (nth-value 0 (compile-sql
                              (alter-type :mood (add-enum-value "meh" :after "ok"))
                              :dialect d))))
    (ok (search "CREATE TYPE" sql))
    (ok (search "AS ENUM" sql))
    (ok (search "'sad'" sql))
    (ok (search "'happy'" sql))
    (ok (search "ALTER TYPE" alter))
    (ok (search "ADD VALUE" alter))
    (ok (search "'meh'" alter))
    (ok (search "AFTER" alter))))

(deftest create-type-distinct-postgres
  (let ((sql (nth-value 0 (compile-sql (create-type :euros :as :numeric)
                                       :dialect (make-postgres-dialect)))))
    (ok (search "CREATE TYPE" sql))
    (ok (search "AS NUMERIC" sql))))

;;; ---- Vendor SQL gaps ----

(deftest postgres-on-conflict-do-nothing
  (let* ((d (make-postgres-dialect))
         (sql (%sql (insert-into :users (columns :email) (sql-values "a@b.c")
                                 (on-conflict :target '(:email) :action :nothing))
                    d)))
    (%assert-contains sql "INSERT INTO" "ON CONFLICT" "DO NOTHING")
    (ok (search "\"email\"" sql))))

(deftest postgres-on-conflict-do-update-returning
  (let* ((d (make-postgres-dialect))
         (sql (%sql (insert-into :users
                      (columns :email :name)
                      (sql-values "a@b.c" "ada")
                      (on-conflict :target '(:email)
                                   :action :update
                                   :set (list (:= :name "ada")))
                      (returning :id :email))
                    d)))
    (%assert-contains sql "ON CONFLICT" "DO UPDATE SET" "RETURNING")
    (ok (< (search "ON CONFLICT" sql) (search "RETURNING" sql)))))

(deftest postgres-on-conflict-constraint
  (let ((sql (%sql (insert-into :t (columns :a) (sql-values 1)
                                (on-conflict :constraint :t-a-key :action :nothing)))))
    (%assert-contains sql "ON CONFLICT ON CONSTRAINT" "DO NOTHING")))

(deftest postgres-copy-csv-stdin
  (let ((sql (%sql (copy-table :users :columns '(:id :name)
                               :direction :from :source :stdin
                               :format :csv
                               :options '(:header t :delimiter ",")))))
    (%assert-contains sql "COPY" "FROM STDIN" "WITH" "FORMAT CSV" "HEADER" "DELIMITER")))

(deftest postgres-copy-to-stdout
  (let ((sql (%sql (copy-table :users :direction :to :source :stdout :format :text))))
    (%assert-contains sql "COPY" "TO STDOUT" "FORMAT TEXT")))

(deftest postgres-materialized-view
  (let* ((d (make-postgres-dialect))
         (create (%sql (create-materialized-view :mv
                          (select (columns :id) (from :t))
                          :if-not-exists t)
                       d))
         (drop (%sql (drop-materialized-view :mv :if-exists t :cascade t) d))
         (refresh (%sql (refresh-materialized-view :mv :concurrently t) d)))
    (%assert-contains create "CREATE MATERIALIZED VIEW" "IF NOT EXISTS" "WITH DATA")
    (%assert-contains drop "DROP MATERIALIZED VIEW" "IF EXISTS" "CASCADE")
    (%assert-contains refresh "REFRESH MATERIALIZED VIEW" "CONCURRENTLY")))

(deftest postgres-partition-by-and-partition-of
  (let* ((d (make-postgres-dialect))
         (parent (%sql (create-table :meas
                          (column :logdate :type :date)
                          (column :peaktemp :type :integer)
                          (partition-by :range :logdate))
                       d))
         (child (%sql (create-table-partition-of :meas-y2024 :meas
                          :for-values '(:from ("2024-01-01") :to ("2025-01-01")))
                      d))
         (list-part (%sql (create-table-partition-of :meas-eu :meas
                              :for-values '(:in "EU" "UK"))
                          d))
         (hash-part (%sql (create-table-partition-of :meas-h0 :meas
                              :for-values '(:modulus 4 :remainder 0))
                          d)))
    (%assert-contains parent "PARTITION BY RANGE")
    (%assert-contains child "PARTITION OF" "FOR VALUES FROM" "TO")
    (%assert-contains list-part "FOR VALUES IN")
    (%assert-contains hash-part "MODULUS" "REMAINDER")))

(deftest postgres-create-trigger-execute-function
  (let ((sql (%sql (create-trigger :trg
                     :timing :after
                     :events '(:insert)
                     :table :users
                     :for-each :row
                     :function :users-audit))))
    (%assert-contains sql "CREATE TRIGGER" "AFTER INSERT" "FOR EACH ROW"
                      "EXECUTE FUNCTION")))

(deftest postgres-for-share-and-strengths
  (let ((d (make-postgres-dialect)))
    (%assert-contains (%sql (select (columns :id) (from :t) (for-share)) d)
                      "FOR SHARE")
    (%assert-contains (%sql (select (columns :id) (from :t)
                                    (for-update :strength :key-share :nowait t))
                            d)
                      "FOR KEY SHARE" "NOWAIT")
    (%assert-contains (%sql (select (columns :id) (from :t)
                                    (for-update :strength :no-key-update))
                            d)
                      "FOR NO KEY UPDATE")))

(deftest postgres-vendor-extension-registry
  (ok (find-sql-extension :on-conflict))
  (ok (find-sql-extension :copy-table))
  (ok (find-sql-extension :partition-by))
  (ok (typep (make-sql-extension :on-conflict :action :nothing) 'on-conflict-clause)))
