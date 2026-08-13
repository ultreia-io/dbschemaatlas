SELECT kcu.table_schema AS schema_name,
       kcu.table_name AS table_name,
       kcu.column_name AS column_name,
       kcu.ordinal_position AS position
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kcu
  ON kcu.constraint_schema = tc.constraint_schema
 AND kcu.constraint_name = tc.constraint_name
 AND kcu.table_schema = tc.table_schema
 AND kcu.table_name = tc.table_name
WHERE tc.constraint_type = 'PRIMARY KEY'
  AND tc.table_schema IN ($1)
ORDER BY kcu.table_schema, kcu.table_name, kcu.ordinal_position
