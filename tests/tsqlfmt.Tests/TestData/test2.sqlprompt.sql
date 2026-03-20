ALTER FUNCTION [aa].[f_Lookup_Conversion_Rate] (
    @Source_Code VARCHAR(3),
    @Source_Frequency INT, -- Number of times per year
    @Target_Code VARCHAR(3),
    @Target_Frequency INT
)
RETURNS TABLE
AS
RETURN SELECT ir.Source_Code,
           ir.Source_Base_Rate,
           ir.Source_Adj_Rate,
           ir.Target_Code,
           ir.Target_Base_Rate,
           ir.Target_Adj_Rate
       FROM aa.f_Detail_Lookup_Conversion_Rate(@Source_Code, @Source_Frequency, @Target_Code, @Target_Frequency) ir