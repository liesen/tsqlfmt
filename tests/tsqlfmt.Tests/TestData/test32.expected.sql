SELECT 1;
GO
CREATE PROCEDURE dbo.test32
AS
BEGIN
    SET @value = 2;

    SELECT @value;

    SET @value = @value + 1;
END;
