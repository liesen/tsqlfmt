create procedure dbo.test33
as
begin
    select customer_id, sum(case when status = 'paid' and invoice_total > 100 then invoice_total when status = 'pending' and pending_total > 100 then pending_total when status = 'cancelled' and cancelled_on is not null then 0 else disputed_total + adjustment_total end) as total_value from dbo.invoices group by customer_id;
end;
