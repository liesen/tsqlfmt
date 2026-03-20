SELECT Cast(pos.upload_date AS DATE) AS 'Date',
    b.job_name AS 'Job',
    pos.id AS 'Positions',
    portfolio_name AS 'Portfolio',
    pos_type AS 'Position Type',
    system AS 'System',
    error AS 'ERROR',
    pos_xml
FROM [MYDB].[dbo].[result_data] (NOLOCK) a
INNER JOIN [MYDB].[dbo].[run_log] (NOLOCK) b
    ON a.run_id = b.run_id
INNER JOIN [MYDB].[dbo].[position] (NOLOCK) pos
    ON pos.internal_id = a.internal_pos_id
WHERE b.run_id IN (
          SELECT MAX(run_id)
          FROM [MYDB].[dbo].[run_log] (NOLOCK)
          WHERE job_name IN ('Daily batch process (scheduled)')
              AND upload_date = '20260101'
          GROUP BY job_name
      )
    AND error IS NOT NULL
UNION ALL
SELECT Cast(pos.upload_date AS DATE) AS 'Date',
    b.job_name AS 'Job',
    pos.id AS 'Positions',
    pos.portfolio_name AS 'Portfolio',
    pos_type AS 'Position Type',
    system AS 'System',
    'Record not processed by batch job' 'ERROR',
    pos_xml
FROM [MYDB].[dbo].[position] (NOLOCK) pos
LEFT JOIN position_result (NOLOCK) pr
    ON pos.upload_date = pr.upload_date
        AND pos.id = pr.id
        AND pos.portfolio_name = pr.portfolio_name
        AND pos.cust_id = pr.cust_id
        AND pr.run_id IN (
                SELECT MAX(run_id)
                FROM run_log (NOLOCK)
                WHERE job_name = 'Daily batch process (scheduled)'
                    AND upload_date = 20260101
            )
INNER JOIN [MYDB].[dbo].[run_log] (NOLOCK) b
    ON pr.run_id = b.run_id
WHERE pos.upload_date = 20260101
    AND pos.cust_id = 1
    AND pr.id IS NULL
    AND pos.archived IS NULL
    AND pos.valid = 1
ORDER BY error,
    portfolio_name,
    id