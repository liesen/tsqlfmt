IF EXISTS (
        SELECT 1
        FROM sys.tables
        WHERE name = 'test24'
    )
    SET @sql = 'DROP TABLE test24'
