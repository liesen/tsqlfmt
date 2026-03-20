SELECT DISTINCT t.category
FROM dbo.products t
WHERE t.is_active = 1
GO

SELECT TOP 25 PERCENT t.id,
    t.name,
    t.score
FROM dbo.candidates t
ORDER BY t.score DESC
GO

SELECT t.region,
    COUNT(*) AS order_count,
    SUM(t.amount) AS total_amount
FROM dbo.orders t
GROUP BY t.region
HAVING COUNT(*) > 100
    AND SUM(t.amount) > 50000
ORDER BY total_amount DESC