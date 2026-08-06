(in-package #:sql-query-postgres)

;;; ---------------------------------------------------------------------------
;;; PostgreSQL vendor AST — ON CONFLICT, COPY, materialized views, partitions
;;; ---------------------------------------------------------------------------

;;; ---- ON CONFLICT (INSERT) ----

(defclass on-conflict-clause (sql-extension sql-clause)
  ((target :initarg :target :reader on-conflict-target :initform nil
           :documentation "NIL, column list, or constraint name keyword/string.")
   (target-constraint :initarg :target-constraint :reader on-conflict-target-constraint
                      :initform nil
                      :documentation "When set, emit ON CONFLICT ON CONSTRAINT name.")
   (action :initarg :action :reader on-conflict-action :initform :nothing
           :documentation ":nothing | :update")
   (set :initarg :set :reader on-conflict-set :initform nil
        :documentation "Assignments for DO UPDATE SET (binary-op or (col val)).")
   (where :initarg :where :reader on-conflict-where :initform nil)))

(defun on-conflict (&key target constraint (action :nothing) set where)
  "INSERT … ON CONFLICT [ (cols) | ON CONSTRAINT name ] DO NOTHING|UPDATE SET …."
  (unless (member action '(:nothing :update))
    (error 'sql-query-error
           :message (format nil "on-conflict :action expects :nothing or :update, got ~s" action)))
  (when (and (eq action :update) (null set))
    (error 'sql-query-error
           :message "on-conflict :update requires :set assignments"))
  (make-instance 'on-conflict-clause
                 :target (when target
                           (mapcar #'ensure-expr (if (listp target) target (list target))))
                 :target-constraint constraint
                 :action action
                 :set (when set
                        (mapcar (lambda (a)
                                  (cond
                                    ((typep a 'binary-op) a)
                                    ((and (consp a) (= 2 (length a)))
                                     (:= (first a) (second a)))
                                    (t (error 'sql-query-error
                                              :message (format nil "on-conflict :set expects := or (col val), got ~s" a)))))
                                (if (and (consp set) (not (typep set 'sql-node))
                                         (or (typep (first set) 'binary-op)
                                             (and (consp (first set)) (= 2 (length (first set))))))
                                    set
                                    (list set))))
                 :where (when where (ensure-expr where))))

(defmethod emit-sql ((dialect postgres-dialect) (clause on-conflict-clause) stream ctx)
  (write-string " ON CONFLICT" stream)
  (cond
    ((on-conflict-target-constraint clause)
     (write-string " ON CONSTRAINT " stream)
     (emit-ident dialect (on-conflict-target-constraint clause) stream))
    ((on-conflict-target clause)
     (write-string " (" stream)
     (emit-column-list dialect (on-conflict-target clause) stream ctx)
     (write-char #\) stream)))
  (ecase (on-conflict-action clause)
    (:nothing (write-string " DO NOTHING" stream))
    (:update
     (write-string " DO UPDATE SET " stream)
     (loop for (a . rest) on (on-conflict-set clause)
           do (emit-sql dialect (binary-op-left a) stream ctx)
              (write-string " = " stream)
              (emit-sql dialect (binary-op-right a) stream ctx)
              (when rest (write-string ", " stream)))
     (when (on-conflict-where clause)
       (write-string " WHERE " stream)
       (emit-sql dialect (on-conflict-where clause) stream ctx)))))

(defmethod emit-insert-extras ((dialect postgres-dialect) stmt stream ctx)
  (let ((oc (find-if (lambda (c) (typep c 'on-conflict-clause))
                     (statement-clauses stmt))))
    (when oc
      (emit-sql dialect oc stream ctx))))

;;; ---- COPY ----

(defclass copy-statement (sql-extension sql-statement)
  ((table :initarg :table :reader copy-statement-table)
   (columns :initarg :columns :reader copy-columns :initform nil)
   (direction :initarg :direction :reader copy-direction
              :documentation ":from | :to")
   (source :initarg :source :reader copy-source
           :documentation ":stdin | :stdout | string path")
   (format :initarg :format :reader copy-format :initform nil
           :documentation ":csv | :text | :binary | string")
   (options :initarg :options :reader copy-options :initform nil
            :documentation "Plist of WITH options (HEADER t, DELIMITER \",\", …).")))

(defun copy-table (table &key columns (direction :from) (source :stdin)
                           format options)
  "COPY table [(cols)] FROM/TO STDIN/STDOUT/'path' [WITH (…)]."
  (unless (member direction '(:from :to))
    (error 'sql-query-error
           :message (format nil "copy-table :direction expects :from/:to, got ~s" direction)))
  (make-instance 'copy-statement
                 :table table
                 :columns (when columns
                            (mapcar #'ensure-expr (if (listp columns) columns (list columns))))
                 :direction direction
                 :source source
                 :format format
                 :options options))

(defun %emit-copy-option (dialect key value stream ctx)
  (write-string (string-upcase (substitute #\_ #\- (symbol-name key))) stream)
  (cond
    ((eq value t))
    ((null value) (write-string " false" stream))
    ((eq value :false) (write-string " false" stream))
    (t
     (write-char #\Space stream)
     (cond
       ((stringp value) (emit-sql dialect (lit value) stream ctx))
       ((symbolp value) (emit-ident dialect value stream))
       ((integerp value) (format stream "~d" value))
       (t (emit-sql dialect (ensure-expr value) stream ctx))))))

(defmethod emit-sql ((dialect postgres-dialect) (stmt copy-statement) stream ctx)
  (write-string "COPY " stream)
  (emit-ident dialect (copy-statement-table stmt) stream)
  (when (copy-columns stmt)
    (write-string " (" stream)
    (emit-column-list dialect (copy-columns stmt) stream ctx)
    (write-char #\) stream))
  (write-char #\Space stream)
  (write-string (ecase (copy-direction stmt) (:from "FROM") (:to "TO")) stream)
  (write-char #\Space stream)
  (let ((src (copy-source stmt)))
    (cond
      ((eq src :stdin) (write-string "STDIN" stream))
      ((eq src :stdout) (write-string "STDOUT" stream))
      ((stringp src) (emit-sql dialect (lit src) stream ctx))
      (t (error 'sql-query-error
                :message (format nil "copy-table :source expects :stdin/:stdout/string, got ~s" src)))))
  (let ((opts (copy-options stmt))
        (fmt (copy-format stmt)))
    (when (or opts fmt)
      (write-string " WITH (" stream)
      (let ((first t))
        (when fmt
          (setf first nil)
          (write-string "FORMAT " stream)
          (write-string (string-upcase
                         (if (keywordp fmt) (symbol-name fmt) (string fmt)))
                        stream))
        (loop for (k v) on opts by #'cddr
              do (unless first (write-string ", " stream))
                 (setf first nil)
                 (%emit-copy-option dialect k v stream ctx)))
      (write-char #\) stream))))

;;; ---- Materialized views ----

(defclass create-materialized-view-statement (sql-extension sql-statement)
  ((name :initarg :name :reader create-materialized-view-name)
   (columns :initarg :columns :reader create-materialized-view-columns :initform nil)
   (query :initarg :query :reader create-materialized-view-query)
   (if-not-exists :initarg :if-not-exists :reader create-materialized-view-if-not-exists
                  :initform nil)
   (with-data :initarg :with-data :reader create-materialized-view-with-data
              :initform t
              :documentation "T = WITH DATA (default); NIL = WITH NO DATA.")))

(defclass drop-materialized-view-statement (sql-extension sql-statement)
  ((name :initarg :name :reader drop-materialized-view-name)
   (if-exists :initarg :if-exists :reader drop-materialized-view-if-exists :initform nil)
   (cascade :initarg :cascade :reader drop-materialized-view-cascade :initform nil)))

(defclass refresh-materialized-view-statement (sql-extension sql-statement)
  ((name :initarg :name :reader refresh-materialized-view-name)
   (concurrently :initarg :concurrently :reader refresh-materialized-view-concurrently
                 :initform nil)
   (with-data :initarg :with-data :reader refresh-materialized-view-with-data
              :initform t)))

(defun create-materialized-view (name query &key columns if-not-exists (with-data t))
  (make-instance 'create-materialized-view-statement
                 :name name
                 :query query
                 :columns (when columns
                            (mapcar #'ensure-expr (if (listp columns) columns (list columns))))
                 :if-not-exists if-not-exists
                 :with-data with-data))

(defun drop-materialized-view (name &key if-exists cascade)
  (make-instance 'drop-materialized-view-statement
                 :name name :if-exists if-exists :cascade cascade))

(defun refresh-materialized-view (name &key concurrently (with-data t))
  (make-instance 'refresh-materialized-view-statement
                 :name name :concurrently concurrently :with-data with-data))

(defmethod emit-sql ((dialect postgres-dialect)
                     (stmt create-materialized-view-statement) stream ctx)
  (write-string "CREATE MATERIALIZED VIEW " stream)
  (when (create-materialized-view-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-materialized-view-name stmt) stream)
  (when (create-materialized-view-columns stmt)
    (write-string " (" stream)
    (emit-column-list dialect (create-materialized-view-columns stmt) stream ctx)
    (write-char #\) stream))
  (write-string " AS " stream)
  (emit-sql dialect (create-materialized-view-query stmt) stream ctx)
  (write-string (if (create-materialized-view-with-data stmt)
                    " WITH DATA"
                    " WITH NO DATA")
                stream))

(defmethod emit-sql ((dialect postgres-dialect)
                     (stmt drop-materialized-view-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "DROP MATERIALIZED VIEW " stream)
  (when (drop-materialized-view-if-exists stmt) (write-string "IF EXISTS " stream))
  (emit-ident dialect (drop-materialized-view-name stmt) stream)
  (when (drop-materialized-view-cascade stmt) (write-string " CASCADE" stream)))

(defmethod emit-sql ((dialect postgres-dialect)
                     (stmt refresh-materialized-view-statement) stream ctx)
  (declare (ignore ctx))
  (write-string "REFRESH MATERIALIZED VIEW " stream)
  (when (refresh-materialized-view-concurrently stmt)
    (write-string "CONCURRENTLY " stream))
  (emit-ident dialect (refresh-materialized-view-name stmt) stream)
  (unless (refresh-materialized-view-with-data stmt)
    (write-string " WITH NO DATA" stream)))

;;; ---- Partitioning ----

(defclass partition-by-extra (sql-extension)
  ((method :initarg :method :reader partition-by-method
           :documentation ":range | :list | :hash")
   (columns :initarg :columns :reader partition-by-columns)))

(defun partition-by (method &rest columns)
  "CREATE TABLE … PARTITION BY RANGE|LIST|HASH (cols) — create-table extra."
  (unless (member method '(:range :list :hash))
    (error 'sql-query-error
           :message (format nil "partition-by method expects :range/:list/:hash, got ~s" method)))
  (unless columns
    (error 'sql-query-error :message "partition-by requires columns"))
  (make-instance 'partition-by-extra
                 :method method
                 :columns (mapcar #'ensure-expr columns)))

(defmethod emit-create-table-extra ((dialect postgres-dialect)
                                    (extra partition-by-extra) stream ctx)
  (write-string " PARTITION BY " stream)
  (write-string (ecase (partition-by-method extra)
                  (:range "RANGE")
                  (:list "LIST")
                  (:hash "HASH"))
                stream)
  (write-string " (" stream)
  (emit-column-list dialect (partition-by-columns extra) stream ctx)
  (write-char #\) stream))

(defclass create-table-partition-of-statement (sql-extension sql-statement)
  ((table :initarg :table :reader create-table-partition-of-table)
   (parent :initarg :parent :reader create-table-partition-of-parent)
   (bound :initarg :bound :reader create-table-partition-of-bound
          :documentation ":from/:to plist, :in list, :modulus/:remainder, or :default")
   (if-not-exists :initarg :if-not-exists
                  :reader create-table-partition-of-if-not-exists :initform nil)))

(defun create-table-partition-of (table parent &key for-values if-not-exists)
  "CREATE TABLE child PARTITION OF parent FOR VALUES …."
  (unless for-values
    (error 'sql-query-error :message "create-table-partition-of requires :for-values"))
  (make-instance 'create-table-partition-of-statement
                 :table table
                 :parent parent
                 :bound for-values
                 :if-not-exists if-not-exists))

(defun %emit-partition-bound (dialect bound stream ctx)
  (cond
    ((eq bound :default)
     (write-string "DEFAULT" stream))
    ((and (consp bound) (eq (first bound) :from))
     ;; (:from (lo…) :to (hi…))
     (let ((from (getf bound :from))
           (to (getf bound :to)))
       (write-string "FROM (" stream)
       (loop for (v . rest) on (if (listp from) from (list from))
             do (emit-sql dialect (ensure-expr v) stream ctx)
                (when rest (write-string ", " stream)))
       (write-string ") TO (" stream)
       (loop for (v . rest) on (if (listp to) to (list to))
             do (emit-sql dialect (ensure-expr v) stream ctx)
                (when rest (write-string ", " stream)))
       (write-char #\) stream)))
    ((and (consp bound) (eq (first bound) :in))
     (write-string "IN (" stream)
     (loop for (v . rest) on (rest bound)
           do (emit-sql dialect (ensure-expr v) stream ctx)
              (when rest (write-string ", " stream)))
     (write-char #\) stream))
    ((and (consp bound) (getf bound :modulus))
     (format stream "WITH (MODULUS ~d, REMAINDER ~d)"
             (getf bound :modulus) (getf bound :remainder)))
    (t (error 'sql-query-error
              :message (format nil "unsupported partition FOR VALUES bound ~s" bound)))))

(defmethod emit-sql ((dialect postgres-dialect)
                     (stmt create-table-partition-of-statement) stream ctx)
  (write-string "CREATE TABLE " stream)
  (when (create-table-partition-of-if-not-exists stmt)
    (write-string "IF NOT EXISTS " stream))
  (emit-ident dialect (create-table-partition-of-table stmt) stream)
  (write-string " PARTITION OF " stream)
  (emit-ident dialect (create-table-partition-of-parent stmt) stream)
  (write-string " FOR VALUES " stream)
  (%emit-partition-bound dialect (create-table-partition-of-bound stmt) stream ctx))

;;; ---- Trigger EXECUTE FUNCTION (PG14+) ----

(defmethod emit-trigger-execute ((dialect postgres-dialect) stmt stream ctx)
  (cond
    ((create-trigger-function stmt)
     ;; Prefer EXECUTE FUNCTION (PG14+); still accepted as PROCEDURE synonym.
     (write-string "EXECUTE FUNCTION " stream)
     (emit-ident dialect (create-trigger-function stmt) stream)
     (write-char #\( stream)
     (loop for (a . rest) on (create-trigger-function-args stmt)
           do (emit-sql dialect (ensure-expr a) stream ctx)
              (when rest (write-string ", " stream)))
     (write-char #\) stream))
    (t (call-next-method))))

;;; ---- Extension registry ----

(defun register-postgres-vendor-extensions ()
  (register-sql-extension :on-conflict #'on-conflict
                          :kind :node
                          :documentation "INSERT … ON CONFLICT")
  (register-sql-extension :copy-table #'copy-table
                          :kind :statement
                          :documentation "COPY … FROM/TO")
  (register-sql-extension :create-materialized-view #'create-materialized-view
                          :kind :statement)
  (register-sql-extension :drop-materialized-view #'drop-materialized-view
                          :kind :statement)
  (register-sql-extension :refresh-materialized-view #'refresh-materialized-view
                          :kind :statement)
  (register-sql-extension :partition-by #'partition-by
                          :kind :create-table-extra)
  (register-sql-extension :create-table-partition-of #'create-table-partition-of
                          :kind :statement)
  t)

(register-postgres-vendor-extensions)
