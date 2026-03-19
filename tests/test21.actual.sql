if @mode = 1
begin
    declare @counter int = 0;
    while @counter < 10
    begin
        set @counter = @counter + 1;
        print 'Iteration: ' + cast(@counter as varchar(10));
    end
end
else
begin
    begin try
        set @result = @input / @divisor;
        print 'Success';
    end try
    begin catch
        set @result = -1;
        print 'Error: ' + error_message();
    end catch
end
