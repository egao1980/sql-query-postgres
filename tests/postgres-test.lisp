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
