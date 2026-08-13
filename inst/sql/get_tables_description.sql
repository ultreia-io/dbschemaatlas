SELECT t.table_schema AS schema_name,
       t.table_name AS table_name,
       obj_description(c.oid, 'pg_class') AS description
FROM information_schema.tables t
JOIN pg_class c ON c.relname = t.table_name
JOIN pg_namespace n ON n.oid = c.relnamespace AND n.nspname = t.table_schema
WHERE t.table_schema IN ($1)
  AND t.table_type = 'BASE TABLE'
ORDER BY t.table_schema, t.table_name
