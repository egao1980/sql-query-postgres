(in-package #:sql-query-postgres/tests)

(defun %compile (stmt &optional (dialect (make-postgres-dialect)))
  (multiple-value-list (compile-sql stmt :dialect dialect)))

(defun %sql (stmt &optional (dialect (make-postgres-dialect)))
  (first (%compile stmt dialect)))

(defun %params (stmt &optional (dialect (make-postgres-dialect)))
  (second (%compile stmt dialect)))

(defun %norm (sql)
  (with-output-to-string (o)
    (let ((space nil))
      (loop for c across (string-upcase sql)
            do (if (member c '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                   (unless space
                     (write-char #\Space o)
                     (setf space t))
                   (progn
                     (write-char c o)
                     (setf space nil)))))))

(defun %has (sql fragment)
  (search (%norm fragment) (%norm sql) :test #'char=))

(defun %assert-contains (sql &rest fragments)
  (dolist (f fragments)
    (ok (%has sql f) (format nil "expected ~s in~%~a" f sql))))

(defun %assert-absent (sql &rest fragments)
  (dolist (f fragments)
    (ng (%has sql f) (format nil "did not expect ~s in~%~a" f sql))))
