SELECT t.id,
    CASE
        WHEN t.status = 1 THEN 'Active'
        WHEN t.status = 2 THEN 'Pending'
        WHEN t.status = 3 THEN 'Suspended'
    ELSE 'Unknown'
    END AS status_label,
    COALESCE(t.display_name, t.login_name, 'N/A') AS resolved_name,
    NULLIF(t.error_code, 0) AS meaningful_error,
    IIF(t.balance > 0, 'Credit', 'Debit') AS balance_type,
    TRY_CAST(t.raw_value AS DECIMAL(18, 4)) AS parsed_value,
    CONVERT(VARCHAR(10), t.created_date, 120) AS date_string
FROM dbo.transactions t