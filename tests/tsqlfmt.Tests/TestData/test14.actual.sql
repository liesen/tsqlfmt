delete t from dbo.staging_records t inner join dbo.import_batches b on b.batch_id = t.batch_id where b.status = 'Processed' and t.created_date < dateadd(day, -90, getutcdate())
