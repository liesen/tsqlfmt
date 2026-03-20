select distinct t.category from dbo.products t where t.is_active = 1
GO
select top 25 percent t.id, t.name, t.score from dbo.candidates t order by t.score desc
GO
select t.region, count(*) as order_count, sum(t.amount) as total_amount from dbo.orders t group by t.region having count(*) > 100 and sum(t.amount) > 50000 order by total_amount desc
