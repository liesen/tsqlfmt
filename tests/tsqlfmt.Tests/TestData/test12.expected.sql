-- Leading comment before first statement
-- Second leading comment line
SELECT 1 AS a,
    2 AS b,
    3 AS c

SELECT t.Id,
    t.Name
FROM dbo.MyTable t
WHERE t.Id > 0
    AND t.Name IS NOT NULL
