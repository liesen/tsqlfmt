IF @mode = 1
BEGIN
    DECLARE @counter INT = 0;

    WHILE @counter < 10
    BEGIN
        SET @counter = @counter + 1;

        PRINT 'Iteration: ' + CAST(@counter AS VARCHAR(10));
    END
END
ELSE
BEGIN
    BEGIN TRY
        SET @result = @input / @divisor;

        PRINT 'Success';
    END TRY
    BEGIN CATCH
        SET @result = -1;

        PRINT 'Error: ' + ERROR_MESSAGE();
    END CATCH
END
