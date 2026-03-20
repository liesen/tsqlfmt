IF @mode = 1
BEGIN
    DECLARE @counter INT = 0;

    WHILE @counter < 10
    BEGIN
        SET @counter = @counter + 1;

        PRINT 'Iteration: ' + cast(@counter AS VARCHAR(10));
    END
END
ELSE
BEGIN
    BEGIN try
        SET @result = @input / @divisor;

        PRINT 'Success';
    END try
    BEGIN catch
        SET @result = -1;

        PRINT 'Error: ' + ERROR_MESSAGE();
    END catch
END