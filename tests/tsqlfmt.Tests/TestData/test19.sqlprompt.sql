SELECT a.id,
    b.value,
    c.name,
    d.result
FROM dbo.base_data a
CROSS JOIN dbo.reference_values b
INNER JOIN dbo.categories c
    ON c.id = a.category_id
CROSS apply (
    SELECT TOP 1 x.result
    FROM dbo.calculations x
    WHERE x.input_id = a.id
    ORDER BY x.created_date DESC
) d
OUTER apply (
    SELECT COUNT(*) AS child_count
    FROM dbo.child_records cr
    WHERE cr.parent_id = a.id
) e
WHERE a.is_active = 1