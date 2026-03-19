SELECT t.department_id,
    t.employee_name,
    t.salary,
    ROW_NUMBER() OVER (partition BY t.department_id
                       ORDER BY t.salary DESC
                 ) AS dept_rank,
    SUM(t.salary) OVER (partition BY t.department_id) AS dept_total,
    AVG(t.salary) OVER () AS company_avg,
    LAG(t.salary, 1) OVER (partition BY t.department_id
                           ORDER BY t.hire_date
                     ) AS prev_salary,
    SUM(t.salary) OVER (partition BY t.department_id
                        ORDER BY t.hire_date
                        rows BETWEEN unbounded preceding AND CURRENT row
                  ) AS running_total
FROM dbo.employees t