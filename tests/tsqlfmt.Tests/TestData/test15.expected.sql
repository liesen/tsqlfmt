;WITH ActiveAccounts AS (
    SELECT a.account_id,
        a.account_name,
        a.region_id
    FROM dbo.accounts a
    WHERE a.is_active = 1
),
RegionTotals AS (
    SELECT aa.region_id,
        COUNT(*) AS account_count,
        r.region_name
    FROM ActiveAccounts aa
    INNER JOIN dbo.regions r
        ON r.region_id = aa.region_id
    GROUP BY aa.region_id,
        r.region_name
)
SELECT rt.region_name,
    rt.account_count
FROM RegionTotals rt
WHERE rt.account_count > 10
ORDER BY rt.account_count DESC
