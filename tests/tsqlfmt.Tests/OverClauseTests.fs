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

[<Fact>]
let ``aggregate with long OVER contents can break inside parentheses while keeping OVER attached`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeSubsequentItemsOnNewLines = PlaceOnNewLine.Always }
            whitespace =
                { config.whitespace with
                    wrapLinesLongerThan = 60 } }

    let sql =
        "select sum(amount) over (partition by customer_id, region_id, territory_id order by order_date, invoice_id) from sales"

    let expected =
        """
SELECT SUM(amount) OVER (
        PARTITION BY customer_id,
            region_id,
            territory_id
        ORDER BY order_date,
            invoice_id
    )
FROM sales
"""

    assertFormatsToWithConfig testConfig expected sql

[<Fact>]
let ``aggregate with long OVER contents follows sqlprompt-style clause breaks`` () =
    let testConfig =
        { config with
            whitespace =
                { config.whitespace with
                    wrapLinesLongerThan = 60 } }

    let sql =
        "select sum(amount) over (partition by department_id order by hire_date rows between unbounded preceding and current row) from sales"

    let expected =
        """
SELECT SUM(amount) OVER (
        PARTITION BY department_id
        ORDER BY hire_date
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
    )
FROM sales
"""

    assertFormatsToWithConfig testConfig expected sql
