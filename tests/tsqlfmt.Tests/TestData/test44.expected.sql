CREATE FUNCTION dbo.fn_test44(@n INT)
RETURNS TABLE
AS
RETURN WITH nums AS (
    SELECT 1 AS n

    UNION ALL

    SELECT n + 1
    FROM nums
    WHERE n < @n
)
SELECT n
FROM nums
