module OverClauseTests

open Xunit
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``aggregate keeps OVER on same line when expression fits`` () =
    let sql = "select sum(amount) over (partition by customer_id) from sales"

    let expected =
        """
SELECT SUM(amount) OVER (PARTITION BY customer_id)
FROM sales
"""

    assertFormatsTo expected sql

[<Fact>]
let ``aggregate with OVER stays grouped on one line inside select list when it fits`` () =
    let sql =
        "select row_number() over (partition by department_id order by salary desc) as dept_rank from employees"

    let expected =
        """
SELECT ROW_NUMBER() OVER (PARTITION BY department_id ORDER BY salary DESC) AS dept_rank
FROM employees
"""

    assertFormatsTo expected sql
