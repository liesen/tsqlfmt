DELETE t
FROM dbo.staging_records t
INNER JOIN dbo.import_batches b
    ON b.batch_id = t.batch_id
WHERE b.status = 'Processed'
    AND t.created_date < DATEADD(day, -90, getutcdate())