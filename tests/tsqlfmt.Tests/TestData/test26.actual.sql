create function dbo.fn_test26() returns TABLE as return
select case @@SERVERNAME when 'A' THEN 65 when 'B' then 66 END as col0, 25 as col1, 'test' as col2