UPDATE t
SET t.status = 'Active',
    t.modified_date = getutcdate(),
    t.modified_by = @userId
FROM dbo.accounts t
INNER JOIN dbo.account_types at
    ON at.type_id = t.type_id
LEFT JOIN dbo.regions r
    ON r.region_id = t.region_id
WHERE t.is_deleted = 0
    AND at.category = 'Premium'
    AND r.country_code = 'SE'