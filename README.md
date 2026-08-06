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

## Vendor SQL

- `on-conflict` — `INSERT … ON CONFLICT … DO NOTHING|UPDATE` (composes with `returning`)
- `copy-table` — `COPY … FROM/TO STDIN/STDOUT/'path' WITH (…)`
- `create-materialized-view` / `drop-materialized-view` / `refresh-materialized-view`
- `partition-by` (create-table extra) + `create-table-partition-of`
- `CREATE TRIGGER` → `EXECUTE FUNCTION` (via core `create-trigger`)
- `for-share` / `for-update :strength` lock strengths


Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

## License

MIT — see [LICENSE](LICENSE).
