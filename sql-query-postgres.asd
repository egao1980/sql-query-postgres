(defsystem "sql-query-postgres"
  :version "0.1.0"
  :description "sql-query dialect backend — PostgreSQL"
  :author "egao1980"
  :license "MIT"
  :depends-on ("sql-query")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "extensions")
               (:file "dialect"))
  :in-order-to ((test-op (test-op "sql-query-postgres/tests"))))

(defsystem "sql-query-postgres/tests"
  :depends-on ("sql-query-postgres" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "postgres-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
