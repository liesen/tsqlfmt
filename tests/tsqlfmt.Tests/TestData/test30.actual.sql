create procedure dbo.test30
    @CustomerId int,
    @Status nvarchar(20) -- status filter
as
begin
    declare @AttemptCount int = 0 -- initialize retries
    set @AttemptCount = @AttemptCount + 1 -- increment retries
    select @CustomerId, @Status -- emit current values
end
