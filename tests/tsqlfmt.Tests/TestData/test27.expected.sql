CREATE FUNCTION dbo.fn_test27(@param INT)
RETURNS TABLE
AS
RETURN (
    WITH cte AS (
        SELECT @param AS col1
    )
    SELECT col1,
        'test' AS col2
    FROM cte
)