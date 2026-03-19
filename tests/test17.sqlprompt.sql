SELECT t.id,
    t.name
FROM dbo.items t
WHERE EXISTS (
    SELECT 1
    FROM dbo.orders o
    WHERE o.item_id = t.id
        AND o.status = 'Shipped'
)
    AND t.name LIKE '%widget%'
    AND t.name NOT LIKE 'TEST%'
    AND t.price BETWEEN 10.00 AND 99.99
    AND t.category_id NOT IN (
            SELECT c.id
            FROM dbo.categories c
            WHERE c.is_discontinued = 1
        )
    AND t.id NOT BETWEEN 1000 AND 2000