# sql-query-postgres

PostgreSQL **dialect backend** for [`sql-query`](https://github.com/egao1980/sql-query) (cl-stack).

Separate project — same pattern as `sql-protocol` / `sql-backend-*`.

```lisp
(asdf:load-system "sql-query-postgres")

(compile-sql
 (select (columns (sql-query-postgres:jsonb-text :payload "name"))
         (from :events)
         (where (sql-query-postgres:jsonb-contains
                 :payload (sql-query:typed "{\"ok\":true}" :jsonb))))
 :dialect (sql-query-postgres:make-postgres-dialect))
```

Registers dialect `:postgres` (`$n` params), plpgsql procedures, and jsonb/array/datetime seeds + helpers.

Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

## License

MIT — see [LICENSE](LICENSE).
