SELECT con.conname AS foreign_key,
       src_ns.nspname AS schema_name,
       src_tbl.relname AS table_name,
       src_col.attname AS column_name,
       target_ns.nspname AS target_schema,
       target_tbl.relname AS target_table,
       target_col.attname AS target_column,
       src_colnum.ord AS column_order
FROM pg_constraint con
JOIN pg_class src_tbl ON src_tbl.oid = con.conrelid
JOIN pg_namespace src_ns ON src_ns.oid = src_tbl.relnamespace
JOIN pg_class target_tbl ON target_tbl.oid = con.confrelid
JOIN pg_namespace target_ns ON target_ns.oid = target_tbl.relnamespace
JOIN unnest(con.conkey) WITH ORDINALITY AS src_colnum(attnum, ord) ON TRUE
JOIN unnest(con.confkey) WITH ORDINALITY AS target_colnum(attnum, ord)
  ON src_colnum.ord = target_colnum.ord
JOIN pg_attribute src_col
  ON src_col.attrelid = src_tbl.oid AND src_col.attnum = src_colnum.attnum
JOIN pg_attribute target_col
  ON target_col.attrelid = target_tbl.oid AND target_col.attnum = target_colnum.attnum
WHERE con.contype = 'f'
  AND src_ns.nspname IN ($1)
ORDER BY src_ns.nspname, src_tbl.relname, con.conname, src_colnum.ord
