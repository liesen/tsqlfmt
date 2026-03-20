CREATE VIEW dbo.v_test28
AS
SELECT customer_id,
    customer_name,
    CASE status
        WHEN 'A' THEN 'Active'
        WHEN 'I' THEN 'Inactive'
        ELSE 'Unknown'
    END AS status_name
FROM dbo.customers
WHERE is_deleted = 0
