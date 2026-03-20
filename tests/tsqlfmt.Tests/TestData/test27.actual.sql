create function dbo.fn_test27(@param INT)
returns TABLE as return (
    with cte as (
        select @param as col1
    )
    select col1, 'test' as col2
    FROM cte
)