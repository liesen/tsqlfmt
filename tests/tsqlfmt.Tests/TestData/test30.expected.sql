CREATE PROCEDURE dbo.test30
    @CustomerId INT,
    @Status NVARCHAR(20) -- status filter
AS
BEGIN
    DECLARE @AttemptCount INT = 0 -- initialize retries

    SET @AttemptCount = @AttemptCount + 1 -- increment retries

    SELECT @CustomerId,
        @Status -- emit current values
END
