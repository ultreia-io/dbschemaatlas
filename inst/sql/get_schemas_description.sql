SELECT n.nspname AS schema_name,
       d.description AS description
FROM pg_namespace n
LEFT JOIN pg_description d
  ON d.objoid = n.oid
 AND d.classoid = 'pg_namespace'::regclass
 AND d.objsubid = 0
WHERE n.nspname IN ($1)
ORDER BY n.nspname
