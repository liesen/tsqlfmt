CREATE VIEW dbo.v_active_orders
AS
SELECT o.order_id,
    o.order_date,
    c.customer_name /* comment customer name */,
    c.email, /* comment email */
    SUM(ol.quantity * ol.unit_price) AS total_amount /* comment total amount */
FROM dbo.orders o
WHERE 1 = 0