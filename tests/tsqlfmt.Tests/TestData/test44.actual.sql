create function dbo.fn_test44(@n int)
returns table as return with nums as (
    select 1 as n
    union all
    select n + 1 from nums where n < @n
)
select n from nums
