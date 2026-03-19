/// Keyword classification and casing logic.
module TSqlFormatter.Keywords

open System.Collections.Generic

let private reservedKeywords =
    HashSet<string>(
        [| "SELECT"; "FROM"; "WHERE"; "JOIN"; "ON"; "AND"; "OR"; "IS"; "NULL"; "NOT"; "IN";
           "BETWEEN"; "AS"; "UNION"; "ALL"; "ORDER"; "BY"; "GROUP"; "HAVING"; "INSERT"; "INTO";
           "VALUES"; "UPDATE"; "SET"; "DELETE"; "MERGE"; "WHEN"; "MATCHED"; "THEN"; "CASE";
           "ELSE"; "END"; "BEGIN"; "IF"; "WHILE"; "TRY"; "CATCH"; "DECLARE"; "RETURN"; "EXISTS";
           "LIKE"; "TOP"; "DISTINCT"; "INNER"; "LEFT"; "RIGHT"; "FULL"; "OUTER"; "CROSS"; "APPLY";
           "WITH"; "NOLOCK"; "ROLLBACK"; "COMMIT"; "TRANSACTION"; "TRAN"; "EXEC"; "EXECUTE";
           "GO"; "CREATE"; "ALTER"; "DROP"; "TABLE"; "VIEW"; "FUNCTION"; "PROCEDURE"; "PROC";
           "TRIGGER"; "INDEX"; "SCHEMA"; "DATABASE"; "USE"; "PRINT"; "RAISERROR"; "THROW";
           "RETURNS"; "OUTPUT"; "READONLY"; "ASC"; "DESC"; "OVER"; "PARTITION"; "ROWS";
           "RANGE"; "UNBOUNDED"; "PRECEDING"; "FOLLOWING"; "CURRENT"; "ROW"; "USING";
           "SOURCE"; "TARGET"; "OPTION"; "RECOMPILE"; "MAXRECURSION"; "PRIMARY"; "KEY";
           "FOREIGN"; "REFERENCES"; "UNIQUE"; "CHECK"; "DEFAULT"; "CONSTRAINT"; "IDENTITY";
           "CLUSTERED"; "NONCLUSTERED"; "INCLUDE"; "FILLFACTOR"; "PAD_INDEX"; "STATISTICS_NORECOMPUTE";
           "SORT_IN_TEMPDB"; "ONLINE"; "ALLOW_ROW_LOCKS"; "ALLOW_PAGE_LOCKS"; "ROWGUIDCOL";
           "TEXTIMAGE_ON"; "FILESTREAM_ON"; "DATA_COMPRESSION"; "COMPUTE"; "BROWSE";
           "HOLDLOCK"; "PAGLOCK"; "ROWLOCK"; "TABLOCK"; "TABLOCKX"; "UPDLOCK"; "XLOCK";
           "SERIALIZABLE"; "READCOMMITTED"; "READCOMMITTEDLOCK"; "READPAST"; "READUNCOMMITTED";
           "REPEATABLEREAD"; "NOWAIT"; "BREAK"; "CONTINUE"; "GOTO"; "LABEL"; "WAITFOR";
           "DELAY"; "TIME"; "BULK"; "OPENROWSET"; "OPENQUERY"; "OPENDATASOURCE";
           "PIVOT"; "UNPIVOT"; "TABLESAMPLE"; "SYSTEM"; "PERCENT"; "REPEATABLE";
           "AT"; "SOME"; "ANY"; "ESCAPE"; "COLLATE"; "CONTAINSTABLE"; "FREETEXTTABLE";
           "FREETEXT"; "CONTAINS"; "OFFSET"; "FETCH"; "NEXT"; "ONLY"; "FIRST"; "LAST";
           "ABSOLUTE"; "RELATIVE"; "SCROLL"; "DYNAMIC"; "FAST_FORWARD"; "FORWARD_ONLY";
           "KEYSET"; "OPTIMISTIC"; "READ_ONLY"; "SCROLL_LOCKS"; "TYPE_WARNING";
           "FOR"; "AFTER"; "INSTEAD"; "OF"; "ENABLE"; "DISABLE"; "ENCRYPTION";
           "SCHEMABINDING"; "NATIVE_COMPILATION"; "CALLED"; "EXTERNAL"; "NAME"; "LANGUAGE";
           "DETERMINISTIC"; "PRECISE"; "DATA_ACCESS"; "NATIVELY_COMPILED";
           "ELSE IF"; "END TRY"; "END CATCH"; "BEGIN TRY"; "BEGIN CATCH";
           "NOT NULL"; "IS NOT"; "IS NULL"; "NOT IN"; "NOT LIKE"; "NOT BETWEEN";
           "NOT EXISTS"; "INNER JOIN"; "LEFT JOIN"; "RIGHT JOIN"; "FULL JOIN";
           "LEFT OUTER JOIN"; "RIGHT OUTER JOIN"; "FULL OUTER JOIN"; "CROSS JOIN";
           "CROSS APPLY"; "OUTER APPLY"; "GROUP BY"; "ORDER BY"; "UNION ALL";
           "INSERT INTO"; "WHEN MATCHED"; "WHEN NOT MATCHED"; "TOP PERCENT" |] :> string seq,
        System.StringComparer.OrdinalIgnoreCase)

let private builtInFunctions =
    HashSet<string>(
        [| "CAST"; "CONVERT"; "ISNULL"; "COALESCE"; "NULLIF"; "IIF"; "CHOOSE";
           "LEN"; "LEFT"; "RIGHT"; "SUBSTRING"; "REPLACE"; "UPPER"; "LOWER";
           "LTRIM"; "RTRIM"; "TRIM"; "CHARINDEX"; "PATINDEX"; "STUFF"; "REVERSE";
           "REPLICATE"; "SPACE"; "STR"; "CHAR"; "ASCII"; "UNICODE"; "NCHAR";
           "QUOTENAME"; "CONCAT"; "CONCAT_WS"; "STRING_SPLIT"; "STRING_ESCAPE";
           "TRANSLATE"; "FORMAT"; "STRING_AGG";
           "DATEADD"; "DATEDIFF"; "DATEDIFF_BIG"; "DATEPART"; "DATENAME";
           "GETDATE"; "GETUTCDATE"; "SYSDATETIME"; "SYSUTCDATETIME"; "SYSDATETIMEOFFSET";
           "SWITCHOFFSET"; "TODATETIMEOFFSET"; "ISDATE"; "EOMONTH"; "DATEFROMPARTS";
           "DATETIME2FROMPARTS"; "DATETIMEFROMPARTS"; "DATETIMEOFFSETFROMPARTS";
           "SMALLDATETIMEFROMPARTS"; "TIMEFROMPARTS";
           "ROUND"; "ABS"; "CEILING"; "FLOOR"; "POWER"; "SIGN"; "SQRT";
           "SQUARE"; "LOG"; "LOG10"; "EXP"; "PI"; "RAND"; "ACOS"; "ASIN"; "ATAN"; "ATN2";
           "COS"; "COT"; "DEGREES"; "RADIANS"; "SIN"; "TAN";
           "ROW_NUMBER"; "RANK"; "DENSE_RANK"; "NTILE"; "LAG"; "LEAD";
           "FIRST_VALUE"; "LAST_VALUE"; "CUME_DIST"; "PERCENT_RANK"; "PERCENTILE_CONT"; "PERCENTILE_DISC";
           "SUM"; "COUNT"; "AVG"; "MIN"; "MAX"; "COUNT_BIG"; "STDEV"; "STDEVP"; "VAR"; "VARP";
           "CHECKSUM_AGG"; "GROUPING"; "GROUPING_ID";
           "NEWID"; "NEWSEQUENTIALID"; "OBJECT_ID"; "OBJECT_NAME"; "DB_ID"; "DB_NAME";
           "SCHEMA_ID"; "SCHEMA_NAME"; "TYPE_ID"; "TYPE_NAME"; "COL_NAME"; "COL_LENGTH";
           "COLUMNPROPERTY"; "DATABASEPROPERTYEX"; "OBJECTPROPERTY"; "OBJECTPROPERTYEX";
           "SERVERPROPERTY"; "TYPEPROPERTY"; "INDEXPROPERTY";
           "SCOPE_IDENTITY"; "IDENT_CURRENT"; "IDENT_INCR"; "IDENT_SEED";
           "ERROR_MESSAGE"; "ERROR_NUMBER"; "ERROR_SEVERITY"; "ERROR_STATE"; "ERROR_LINE"; "ERROR_PROCEDURE";
           "XACT_STATE"; "SESSION_USER"; "SYSTEM_USER"; "CURRENT_USER"; "USER_NAME"; "SUSER_SNAME"; "SUSER_SID";
           "HAS_PERMS_BY_NAME"; "IS_MEMBER"; "IS_ROLEMEMBER"; "IS_SRVROLEMEMBER";
           "TRY_CAST"; "TRY_CONVERT"; "TRY_PARSE"; "PARSE";
           "JSON_VALUE"; "JSON_QUERY"; "JSON_MODIFY"; "ISJSON"; "OPENJSON"; "JSON_OBJECT"; "JSON_ARRAY";
           "GREATEST"; "LEAST"; "GENERATE_SERIES"; "DATE_BUCKET" |] :> string seq,
        System.StringComparer.OrdinalIgnoreCase)

let private builtInDataTypes =
    HashSet<string>(
        [| "INT"; "BIGINT"; "SMALLINT"; "TINYINT"; "BIT";
           "DECIMAL"; "NUMERIC"; "FLOAT"; "REAL"; "MONEY"; "SMALLMONEY";
           "DATE"; "DATETIME"; "DATETIME2"; "DATETIMEOFFSET"; "SMALLDATETIME"; "TIME";
           "CHAR"; "VARCHAR"; "NCHAR"; "NVARCHAR"; "TEXT"; "NTEXT";
           "BINARY"; "VARBINARY"; "IMAGE";
           "UNIQUEIDENTIFIER"; "XML"; "TABLE";
           "SQL_VARIANT"; "GEOGRAPHY"; "GEOMETRY"; "HIERARCHYID"; "SYSNAME";
           "ROWVERSION"; "TIMESTAMP"; "CURSOR" |] :> string seq,
        System.StringComparer.OrdinalIgnoreCase)

let private globalVariables =
    HashSet<string>(
        [| "@@ROWCOUNT"; "@@ERROR"; "@@IDENTITY"; "@@TRANCOUNT"; "@@FETCH_STATUS";
           "@@VERSION"; "@@SERVERNAME"; "@@SPID"; "@@NESTLEVEL"; "@@PROCID";
           "@@DBTS"; "@@LANGID"; "@@LANGUAGE"; "@@LOCK_TIMEOUT"; "@@MAX_CONNECTIONS";
           "@@MAX_PRECISION"; "@@OPTIONS"; "@@REMSERVER"; "@@CONNECTIONS";
           "@@CPU_BUSY"; "@@IDLE"; "@@IO_BUSY"; "@@PACKET_ERRORS"; "@@PACK_RECEIVED";
           "@@PACK_SENT"; "@@TIMETICKS"; "@@TOTAL_ERRORS"; "@@TOTAL_READ"; "@@TOTAL_WRITE" |] :> string seq,
        System.StringComparer.OrdinalIgnoreCase)

/// Applies casing to a word based on the CasingStyle.
let applyCase (style: Config.CasingStyle) (word: string) : string =
    match style with
    | Config.CasingStyle.LeaveAsIs -> word
    | Config.CasingStyle.Uppercase -> word.ToUpperInvariant()
    | Config.CasingStyle.Lowercase -> word.ToLowerInvariant()
    | _ -> word // LowerCamelCase / UpperCamelCase not implemented yet

/// Determine if a token text is a reserved keyword.
let isReservedKeyword (s: string) = reservedKeywords.Contains(s)

/// Determine if a token text is a built-in function name.
let isBuiltInFunction (s: string) = builtInFunctions.Contains(s)

/// Determine if a token text is a built-in data type.
let isBuiltInDataType (s: string) = builtInDataTypes.Contains(s)

/// Determine if a token text is a global variable.
let isGlobalVariable (s: string) = s.StartsWith("@@") && globalVariables.Contains(s)

/// Apply the appropriate casing to a token based on config.
let caseToken (cfg: Config.CasingConfig) (tokenText: string) : string =
    if isGlobalVariable tokenText then
        applyCase cfg.globalVariables tokenText
    elif isBuiltInFunction tokenText then
        applyCase cfg.builtInFunctions tokenText
    elif isBuiltInDataType tokenText then
        applyCase cfg.builtInDataTypes tokenText
    elif isReservedKeyword tokenText then
        applyCase cfg.reservedKeywords tokenText
    else
        tokenText

/// Apply casing to a keyword (known to be a reserved keyword).
let caseKeyword (cfg: Config.CasingConfig) (keyword: string) : string =
    applyCase cfg.reservedKeywords keyword

/// Apply casing to a built-in function name.
let caseFunction (cfg: Config.CasingConfig) (name: string) : string =
    applyCase cfg.builtInFunctions name

/// Apply casing to a built-in data type.
let caseDataType (cfg: Config.CasingConfig) (name: string) : string =
    applyCase cfg.builtInDataTypes name
