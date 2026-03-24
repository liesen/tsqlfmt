CREATE FUNCTION dbo.f_order()
RETURNS TABLE (
    c1 INT,
    c2 INT
)
ORDER (c1 DESC, c2)
AS
EXTERNAL NAME MyAssembly.MyClass.MyMethod
