create view dbo.v_active_orders as
select o.order_id, o.order_date, c.customer_name /* comment customer name */, c.email, /* comment email */
  sum(ol.quantity * ol.unit_price) as total_amount /* comment total amount */
from dbo.orders o
where 1 = 0
