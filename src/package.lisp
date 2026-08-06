(defpackage #:sql-query-postgres
  (:use #:cl #:sql-query)
  (:shadowing-import-from #:sql-query #:count #:union)
  (:export #:postgres-dialect
           #:make-postgres-dialect
           #:use-postgres-dialect
           #:register-postgres-extensions
           ;; helpers
           #:jsonb-ref #:jsonb-text #:jsonb-path #:jsonb-path-text #:jsonb-contains
           #:date-trunc #:date-part #:to-jsonb #:jsonb-build-object #:jsonb-set
           #:array-agg #:unnest #:generate-series #:now
           ;; vendor SQL
           #:on-conflict #:on-conflict-clause
           #:copy-table #:copy-statement #:copy-statement-table
           #:create-materialized-view #:drop-materialized-view #:refresh-materialized-view
           #:create-materialized-view-statement #:drop-materialized-view-statement
           #:refresh-materialized-view-statement
           #:partition-by #:partition-by-extra
           #:create-table-partition-of #:create-table-partition-of-statement
           #:register-postgres-vendor-extensions))

(in-package #:sql-query-postgres)
