(in-package #:sql-query-postgres)

;;; ---------------------------------------------------------------------------
;;; PostgreSQL type / operator / function seeds
;;; Encode/decode are overridable — no JSON/BSON library hard-dep.
;;; ---------------------------------------------------------------------------

(defun %json-encode (value)
  (if (stringp value) value (prin1-to-string value)))

(defun %json-decode (value) value)

(defun %bson-encode (value)
  "Wire BSON as byte vector or already-encoded string; override for real codecs."
  (cond
    ((vectorp value) value)
    ((stringp value) value)
    (t (error "bson encode needs string or vector (got ~s) — register custom :encode"
              value))))

(defun %dt-encode (value)
  "Datetime wire: string kept; integer → unix seconds for to_timestamp paths."
  (ctypecase value
    (string value)
    (integer value)
    (real value)))

(defun %pg-timestamp-to-expr (dialect value)
  (declare (ignore dialect))
  (ctypecase value
    (string (sql-cast (lit value) :timestamptz))
    (integer (sql-func :to-timestamp value))
    (real (sql-func :to-timestamp value))))

(defun %pg-date-to-expr (dialect value)
  (declare (ignore dialect))
  (ctypecase value
    (string (sql-cast (lit value) :date))
    (integer (sql-func :to-timestamp value)) ; cast date downstream if needed
    (t (lit value))))

(defun %pg-array-to-expr (dialect value)
  (declare (ignore dialect))
  (apply #'array-lit (if (listp value) value (coerce value 'list))))

(defun register-postgres-extensions (dialect)
  "Seed PG types, operators, and functions."
  ;; ---- types ----
  (register-sql-type :json dialect :sql "JSON"
    :encode #'%json-encode :decode #'%json-decode)
  (register-sql-type :jsonb dialect :sql "JSONB"
    :encode #'%json-encode :decode #'%json-decode)
  (register-sql-type :bson dialect :sql "BYTEA"
    :encode #'%bson-encode :decode #'identity)
  (register-sql-type :uuid dialect :sql "UUID")
  (register-sql-type :inet dialect :sql "INET")
  (register-sql-type :cidr dialect :sql "CIDR")
  (register-sql-type :macaddr dialect :sql "MACADDR")
  (register-sql-type :bytea dialect :sql "BYTEA")
  (register-sql-type :xml dialect :sql "XML")
  (register-sql-type :tsvector dialect :sql "TSVECTOR")
  (register-sql-type :tsquery dialect :sql "TSQUERY")
  (register-sql-type :interval dialect :sql "INTERVAL")
  (register-sql-type :date dialect :sql "DATE"
    :encode #'%dt-encode :to-expr #'%pg-date-to-expr)
  (register-sql-type :time dialect :sql "TIME" :encode #'%dt-encode)
  (register-sql-type :timetz dialect :sql "TIMETZ" :encode #'%dt-encode)
  (register-sql-type :timestamp dialect :sql "TIMESTAMP"
    :encode #'%dt-encode
    :to-expr (lambda (d v)
               (declare (ignore d))
               (ctypecase v
                 (string (sql-cast (lit v) :timestamp))
                 (integer (sql-func :to-timestamp v))
                 (real (sql-func :to-timestamp v)))))
  (register-sql-type :timestamptz dialect :sql "TIMESTAMPTZ"
    :encode #'%dt-encode :to-expr #'%pg-timestamp-to-expr)
  (register-sql-type :array dialect
    :sql (lambda (d spec)
           (format nil "~a[]" (dialect-type-sql d (or (second spec) :text))))
    :to-expr #'%pg-array-to-expr)
  (register-sql-type :int-array dialect :sql "INTEGER[]"
    :to-expr #'%pg-array-to-expr)
  (register-sql-type :text-array dialect :sql "TEXT[]"
    :to-expr #'%pg-array-to-expr)
  (register-sql-type :jsonb-array dialect :sql "JSONB[]"
    :to-expr #'%pg-array-to-expr)

  ;; ---- operators ----
  (register-sql-op :-> :binary dialect :sql "->")
  (register-sql-op :->> :binary dialect :sql "->>")
  (register-sql-op :|#>| :binary dialect :sql "#>")
  (register-sql-op :|#>>| :binary dialect :sql "#>>")
  (register-sql-op :@> :binary dialect :sql "@>")
  (register-sql-op :<@ :binary dialect :sql "<@")
  (register-sql-op :|?||| :binary dialect :sql "?|")
  (register-sql-op :|?&| :binary dialect :sql "?&")
  (register-sql-op :|||||| :binary dialect :sql "||")
  (register-sql-op :% :binary dialect :sql "%")          ; modulo / match
  (register-sql-op :^ :binary dialect :sql "^")
  (register-sql-op :<< :binary dialect :sql "<<")
  (register-sql-op :>> :binary dialect :sql ">>")
  (register-sql-op :& :binary dialect :sql "&")
  (register-sql-op :|||| :binary dialect :sql "|")       ; bitwise or
  (register-sql-op :~ :binary dialect :sql "~")          ; regex match
  (register-sql-op :~* :binary dialect :sql "~*")
  (register-sql-op :!~ :binary dialect :sql "!~")
  (register-sql-op :!~* :binary dialect :sql "!~*")
  (register-sql-op :@@ :binary dialect :sql "@@")        ; tsvector match
  (register-sql-op :&& :binary dialect :sql "&&")        ; array overlap
  (register-sql-op :|#-| :binary dialect :sql "#-")      ; jsonb delete path

  ;; ---- functions ----
  (register-sql-func :current-date dialect :sql "CURRENT_DATE"
    :emit (lambda (d n s c) (declare (ignore d n c)) (write-string "CURRENT_DATE" s)))
  (register-sql-func :current-timestamp dialect :sql "CURRENT_TIMESTAMP"
    :emit (lambda (d n s c) (declare (ignore d n c)) (write-string "CURRENT_TIMESTAMP" s)))
  (register-sql-func :current-time dialect :sql "CURRENT_TIME"
    :emit (lambda (d n s c) (declare (ignore d n c)) (write-string "CURRENT_TIME" s)))
  (register-sql-func :extract dialect :sql "EXTRACT" :arity 2
    :emit (lambda (dialect node stream ctx)
            (let ((args (function-call-args node)))
              (write-string "EXTRACT(" stream)
              (emit-sql dialect (first args) stream ctx)
              (write-string " FROM " stream)
              (emit-sql dialect (second args) stream ctx)
              (write-char #\) stream))))
  (dolist (pair '((:now . "now")
                  (:clock-timestamp . "clock_timestamp")
                  (:date-trunc . "date_trunc")
                  (:date-part . "date_part")
                  (:age . "age")
                  (:to-timestamp . "to_timestamp")
                  (:to-date . "to_date")
                  (:to-char . "to_char")
                  (:make-interval . "make_interval")
                  (:jsonb-build-object . "jsonb_build_object")
                  (:jsonb-build-array . "jsonb_build_array")
                  (:jsonb-set . "jsonb_set")
                  (:jsonb-insert . "jsonb_insert")
                  (:jsonb-strip-nulls . "jsonb_strip_nulls")
                  (:jsonb-typeof . "jsonb_typeof")
                  (:jsonb-array-length . "jsonb_array_length")
                  (:jsonb-each . "jsonb_each")
                  (:jsonb-object-keys . "jsonb_object_keys")
                  (:jsonb-pretty . "jsonb_pretty")
                  (:to-jsonb . "to_jsonb")
                  (:to-json . "to_json")
                  (:array-agg . "array_agg")
                  (:array-append . "array_append")
                  (:array-cat . "array_cat")
                  (:array-length . "array_length")
                  (:array-remove . "array_remove")
                  (:unnest . "unnest")
                  (:cardinality . "cardinality")
                  (:generate-series . "generate_series")
                  (:coalesce . "coalesce")
                  (:nullif . "nullif")
                  (:greatest . "greatest")
                  (:least . "least")
                  (:concat . "concat")
                  (:format . "format")
                  (:lower . "lower")
                  (:upper . "upper")
                  (:length . "length")
                  (:substr . "substr")
                  (:substring . "substring")
                  (:trim . "trim")
                  (:replace . "replace")
                  (:md5 . "md5")
                  (:gen-random-uuid . "gen_random_uuid")
                  (:to-tsvector . "to_tsvector")
                  (:to-tsquery . "to_tsquery")
                  (:plainto-tsquery . "plainto_tsquery")
                  (:ts-rank . "ts_rank")
                  (:setweight . "setweight")))
    (register-sql-func (car pair) dialect :sql (cdr pair)))
  dialect)

;;; Ergonomic helpers → ops / funcs

(defun jsonb-ref (expr key)
  "expr -> key (JSON object field as JSON)."
  (ensure-expr (list :-> expr key)))

(defun jsonb-text (expr key)
  "expr ->> key (JSON object field as text)."
  (ensure-expr (list :->> expr key)))

(defun jsonb-path (expr path)
  "expr #> path (JSON path as JSON)."
  (ensure-expr (list :|#>| expr path)))

(defun jsonb-path-text (expr path)
  (ensure-expr (list :|#>>| expr path)))

(defun jsonb-contains (expr contained)
  (ensure-expr (list :@> expr contained)))

(defun date-trunc (unit expr)
  (sql-func :date-trunc unit expr))

(defun date-part (field expr)
  (sql-func :date-part field expr))

(defun to-jsonb (expr)
  (sql-func :to-jsonb expr))

(defun jsonb-build-object (&rest kvs)
  (apply #'sql-func :jsonb-build-object kvs))

(defun jsonb-set (target path new-value &optional create-if-missing)
  (if create-if-missing
      (sql-func :jsonb-set target path new-value create-if-missing)
      (sql-func :jsonb-set target path new-value)))

(defun array-agg (expr)
  (sql-func :array-agg expr))

(defun unnest (expr)
  (sql-func :unnest expr))

(defun generate-series (start end &optional step)
  (if step
      (sql-func :generate-series start end step)
      (sql-func :generate-series start end)))

(defun now ()
  (sql-func :now))
