CREATE PROCEDURE dbo.test33
AS
BEGIN
    SELECT customer_id,
        SUM(
            CASE
                WHEN status = 'paid'
                    AND invoice_total > 100 THEN invoice_total
                WHEN status = 'pending'
                    AND pending_total > 100 THEN pending_total
                WHEN status = 'cancelled'
                    AND cancelled_on IS NOT NULL THEN 0
                ELSE disputed_total + adjustment_total
            END
        ) AS total_value
    FROM dbo.invoices
    GROUP BY customer_id;
END;
