create view dbo.v_test28 as
select customer_id, customer_name, case status when 'A' then 'Active' when 'I' then 'Inactive' else 'Unknown' end as status_name
from dbo.customers
where is_deleted = 0
