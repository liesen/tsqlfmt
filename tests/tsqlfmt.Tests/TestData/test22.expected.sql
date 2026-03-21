CREATE PROCEDURE dbo.usp_ProcessRecords
    @batchSize INT = 100,
    @dryRun BIT = 0
AS
BEGIN
    SET NOCOUNT ON;
    
    DECLARE @processed INT = 0;
    
    SELECT TOP (@batchSize) r.id,
        r.status
    INTO #temp
    FROM dbo.pending_records r
    WHERE r.status = 'New'
    ORDER BY r.priority DESC;
    
    IF @dryRun = 0
    BEGIN
        UPDATE t
        SET t.status = 'Processing',
            t.started_date = GETUTCDATE()
        FROM dbo.pending_records t
        INNER JOIN #temp tmp
            ON tmp.id = t.id;
        
        SET @processed = @@ROWCOUNT;
    END
    
    SELECT @processed AS records_processed;
    
    DROP TABLE #temp;
END
GO
CREATE PROCEDURE dbo.usp_CleanupRecords
    @cutoffDays INT = 30
AS
BEGIN
    DELETE dbo.archive_records
    WHERE created_date < DATEADD(day, -@cutoffDays, GETUTCDATE());
END
