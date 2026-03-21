select 1;
go

create procedure dbo.test32
as
begin
    set @value = 2;
    select @value;
    set @value = @value + 1;
end;
