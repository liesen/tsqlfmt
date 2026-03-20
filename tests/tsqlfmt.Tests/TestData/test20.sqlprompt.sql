INSERT INTO dbo.archive_records (
    record_id, record_type, record_date, source_system
)
SELECT t.id,
    t.type_code,
    t.created_date,
    'MainSystem'
FROM dbo.active_records t
INNER JOIN dbo.record_types rt
    ON rt.type_id = t.type_id
WHERE t.created_date < DATEADD(year, -2, getutcdate())
    AND rt.is_archivable = 1
ORDER BY t.created_date