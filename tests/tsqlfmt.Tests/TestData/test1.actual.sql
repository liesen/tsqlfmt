    SELECT  Cast(pos.upload_date as date) as 'Date',
              b.job_name as 'Job',
              pos.id as 'Positions',
              portfolio_name as 'Portfolio',
              pos_type as 'Position Type',
              system as 'System',
              error as 'ERROR',
              pos_xml
  FROM [MYDB].[dbo].[result_data] (NOLOCK) a
  INNER JOIN [MYDB].[dbo].[run_log] (NOLOCK)  b on a.run_id = b.run_id
  INNER JOIN [MYDB].[dbo].[position] (NOLOCK)  pos on pos.internal_id=a.internal_pos_id
  where b.run_id in 
      (
      SELECT MAX(run_id) FROM [MYDB].[dbo].[run_log] (NOLOCK)  
      WHERE job_name in ( 'Daily batch process (scheduled)' )
      AND upload_date= '20260101'
      GROUP BY job_name
      )
         and error is not null
    UNION ALL
    
    SELECT Cast(pos.upload_date as date) as 'Date',
    		b.job_name as 'Job',
    		pos.id as 'Positions',
    		pos.portfolio_name as 'Portfolio',
    		pos_type as 'Position Type',
    		system as 'System',
    		'Record not processed by batch job' 'ERROR',
    		pos_xml
    from [MYDB].[dbo].[position] (NOLOCK)  pos
    
    LEFT JOIN position_result (NOLOCK)  pr 
    		on pos.upload_date=pr.upload_date
    		and pos.id = pr.id 
    		and pos.portfolio_name = pr.portfolio_name 
    		and pos.cust_id = pr.cust_id
    		and pr.run_id in
    			(select MAX(run_id) from run_log (NOLOCK) 
    			where job_name = 'Daily batch process (scheduled)'
    			and upload_date = 20260101)
                
    INNER JOIN [MYDB].[dbo].[run_log] (NOLOCK) b on pr.run_id = b.run_id
    	where pos.upload_date=  20260101
    	and pos.cust_id = 1
    	and pr.id IS NULL
    	and pos.archived IS NULL
    	and pos.valid = 1
    order by error,portfolio_name, id
