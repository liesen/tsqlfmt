-- Leading comment before first statement
-- Second leading comment line
SELECT
    1 AS a,
    /* inline comment */ 2 AS b,
    3 AS c -- trailing comment on column

SELECT
    t.Id,
    t.Name -- trailing comment on last column
FROM
    dbo.MyTable t
WHERE
    t.Id > 0 /* inline in WHERE */
    AND t.Name IS NOT NULL
