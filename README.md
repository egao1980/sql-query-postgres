# sql-query-postgres

PostgreSQL **dialect backend** for [`sql-query`](https://github.com/egao1980/sql-query) (cl-stack).

**Version:** `0.2.0` (requires `sql-query` ≥ 0.2.0 for insert/trigger/lock hooks).  
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

Registers dialect `:postgres` (`$n` params), plpgsql procedures, jsonb/array/datetime seeds + helpers, and:

| Constructor / hook | SQL |
|--------------------|-----|
| `on-conflict` | `INSERT … ON CONFLICT … DO NOTHING\|UPDATE` (+ `returning`) |
| `copy-table` | `COPY … FROM/TO … WITH (…)` |
| `create-materialized-view` / `drop-` / `refresh-` | matviews |
| `partition-by` + `create-table-partition-of` | declarative partitions |
| `create-type` `:enum` / `:kind :base` | ENUM + base-type UDTs |
| core `create-trigger` | emits `EXECUTE FUNCTION` |

Also: `for-share` / `for-update :strength` lock strengths.

Brief: [sql.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/sql.md).

## License

MIT — see [LICENSE](LICENSE).
