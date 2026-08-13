SELECT cols.table_schema AS schema_name,
       cols.table_name AS table_name,
       cols.column_name AS column_name,
       format_type(a.atttypid, a.atttypmod) AS type,
       CASE WHEN cols.is_nullable = 'NO' THEN 'YES' ELSE 'NO' END AS mandatory,
       pgd.description AS description
FROM information_schema.columns cols
JOIN pg_namespace ns ON ns.nspname = cols.table_schema
JOIN pg_class tbl ON tbl.relname = cols.table_name AND tbl.relnamespace = ns.oid
JOIN pg_attribute a ON a.attrelid = tbl.oid AND a.attname = cols.column_name
LEFT JOIN pg_description pgd ON pgd.objoid = tbl.oid AND pgd.objsubid = a.attnum
WHERE cols.table_schema IN ($1)
  AND a.attnum > 0
  AND tbl.relkind = 'r'
  AND NOT a.attisdropped
ORDER BY cols.table_schema, cols.table_name, cols.ordinal_position
