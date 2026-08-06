(defpackage #:sql-query-postgres/tests
  (:use #:cl #:rove #:sql-query #:sql-query-postgres)
  (:shadowing-import-from #:sql-query #:count #:union)
  (:export #:%sql #:%params #:%compile #:%has #:%norm
           #:%assert-contains #:%assert-absent))

(in-package #:sql-query-postgres/tests)
