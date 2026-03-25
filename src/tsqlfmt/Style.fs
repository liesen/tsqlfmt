/// Style model matching the formattingstyle-schema.json / SQL Prompt format.
module TSqlFormatter.Style

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

// ─── Enums ───

/// How text is cased.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type CasingStyle =
    | [<JsonPropertyName("leaveAsIs")>] LeaveAsIs = 0
    | [<JsonPropertyName("lowercase")>] Lowercase = 1
    | [<JsonPropertyName("uppercase")>] Uppercase = 2
    | [<JsonPropertyName("lowerCamelCase")>] LowerCamelCase = 3
    | [<JsonPropertyName("upperCamelCase")>] UpperCamelCase = 4

/// The format to use for parenthesized code.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type ParenthesisStyle =
    | [<JsonPropertyName("compactSimple")>] CompactSimple = 0
    | [<JsonPropertyName("compactToStatement")>] CompactToStatement = 1
    | [<JsonPropertyName("compactIndented")>] CompactIndented = 2
    | [<JsonPropertyName("compactRightAligned")>] CompactRightAligned = 3
    | [<JsonPropertyName("expandedSimple")>] ExpandedSimple = 4
    | [<JsonPropertyName("expandedSplit")>] ExpandedSplit = 5
    | [<JsonPropertyName("expandedToStatement")>] ExpandedToStatement = 6
    | [<JsonPropertyName("expandedIndented")>] ExpandedIndented = 7
    | [<JsonPropertyName("expandedRightAligned")>] ExpandedRightAligned = 8

/// When content is placed on a new line.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type PlaceOnNewLine =
    | [<JsonPropertyName("always")>] Always = 0
    | [<JsonPropertyName("never")>] Never = 1
    | [<JsonPropertyName("ifLongerThanMaxLineLength")>] IfLongerThanMaxLineLength = 2
    | [<JsonPropertyName("ifSubsequentValues")>] IfSubsequentValues = 3
    | [<JsonPropertyName("ifSubsequentItems")>] IfSubsequentItems = 4
    | [<JsonPropertyName("ifMultiple")>] IfMultiple = 5
    | [<JsonPropertyName("ifMultipleItems")>] IfMultipleItems = 6
    | [<JsonPropertyName("ifInputExpression")>] IfInputExpression = 7

/// How clauses within DML statements are aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type ClauseAlignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("toFirstListItem")>] ToFirstListItem = 2

/// How items or keywords are aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type Alignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("beforeFirstListItem")>] BeforeFirstListItem = 2
    | [<JsonPropertyName("toFirstListItem")>] ToFirstListItem = 3
    | [<JsonPropertyName("indented")>] Indented = 4

/// How the JOIN keyword is aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type JoinKeywordAlignment =
    | [<JsonPropertyName("toFrom")>] ToFrom = 0
    | [<JsonPropertyName("rightAlignedToFrom")>] RightAlignedToFrom = 1
    | [<JsonPropertyName("toTable")>] ToTable = 2
    | [<JsonPropertyName("indented")>] Indented = 3

/// How the ON keyword is aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type OnKeywordAlignment =
    | [<JsonPropertyName("toJoin")>] ToJoin = 0
    | [<JsonPropertyName("rightAlignedToJoin")>] RightAlignedToJoin = 1
    | [<JsonPropertyName("rightAlignedToInner")>] RightAlignedToInner = 2
    | [<JsonPropertyName("toTable")>] ToTable = 3
    | [<JsonPropertyName("indented")>] Indented = 4

/// How the WHEN keywords are aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type WhenAlignment =
    | [<JsonPropertyName("indentedFromCase")>] IndentedFromCase = 0
    | [<JsonPropertyName("toCase")>] ToCase = 1
    | [<JsonPropertyName("toFirstItem")>] ToFirstItem = 2

/// How the END keyword is aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type EndAlignment =
    | [<JsonPropertyName("toCase")>] ToCase = 0
    | [<JsonPropertyName("toWhen")>] ToWhen = 1
    | [<JsonPropertyName("rightAlignedToWhen")>] RightAlignedToWhen = 2

/// How the IN keyword is aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type InAlignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("indented")>] Indented = 2

/// How commas separating list items are aligned.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type CommaAlignment =
    | [<JsonPropertyName("beforeItem")>] BeforeItem = 0
    | [<JsonPropertyName("toList")>] ToList = 1
    | [<JsonPropertyName("toStatement")>] ToStatement = 2

/// Whether spaces or tabs are used while formatting.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type SpacesOrTabs =
    | [<JsonPropertyName("spaces")>] Spaces = 0
    | [<JsonPropertyName("tabs")>] Tabs = 1
    | [<JsonPropertyName("tabsIfPossible")>] TabsIfPossible = 2

/// Whether spaces will be inserted before semicolons.
[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type WhiteSpaceBeforeSemiColon =
    | [<JsonPropertyName("none")>] None = 0
    | [<JsonPropertyName("spaceBefore")>] SpaceBefore = 1
    | [<JsonPropertyName("newLineBefore")>] NewLineBefore = 2

// ─── Style Records ───

/// Formatting options for new lines.
type NewLines =
    {
        /// Preserve existing empty lines between statements.
        [<JsonPropertyName("preserveExistingEmptyLinesBetweenStatements")>]
        preserveExistingEmptyLinesBetweenStatements: bool
        /// Preserve existing empty lines after batch separator.
        [<JsonPropertyName("preserveExistingEmptyLinesAfterBatchSeparator")>]
        preserveExistingEmptyLinesAfterBatchSeparator: bool
        /// How many empty lines to insert between separate statements.
        [<JsonPropertyName("emptyLinesBetweenStatements")>]
        emptyLinesBetweenStatements: int
        /// How many empty lines to insert after a batch separator.
        [<JsonPropertyName("emptyLinesAfterBatchSeparator")>]
        emptyLinesAfterBatchSeparator: int
    }

/// Formatting options for whitespace.
type Whitespace =
    {
        /// Whether spaces or tabs are used while formatting.
        [<JsonPropertyName("spacesOrTabs")>]
        spacesOrTabs: SpacesOrTabs
        /// How many spaces are inserted when tab is pressed.
        [<JsonPropertyName("numberOfSpacesInTabs")>]
        numberOfSpacesInTabs: int
        /// Whether long lines will be wrapped onto a new line.
        [<JsonPropertyName("wrapLongLines")>]
        wrapLongLines: bool
        /// Lines with more than this number of characters will be wrapped.
        [<JsonPropertyName("wrapLinesLongerThan")>]
        wrapLinesLongerThan: int
        /// Whether spaces will be inserted before semicolons.
        [<JsonPropertyName("whiteSpaceBeforeSemiColon")>]
        whiteSpaceBeforeSemiColon: WhiteSpaceBeforeSemiColon
        /// Formatting options for new lines.
        [<JsonPropertyName("newLines")>]
        newLines: NewLines
    }

/// Formatting options for lists.
type Lists =
    {
        /// Place first item in list on new line.
        [<JsonPropertyName("placeFirstItemOnNewLine")>]
        placeFirstItemOnNewLine: PlaceOnNewLine
        /// Place subsequent list items on new lines.
        [<JsonPropertyName("placeSubsequentItemsOnNewLines")>]
        placeSubsequentItemsOnNewLines: PlaceOnNewLine
        /// Align subsequent list items with first item.
        [<JsonPropertyName("alignSubsequentItemsWithFirstItem")>]
        alignSubsequentItemsWithFirstItem: bool
        /// Align list items across clauses.
        [<JsonPropertyName("alignItemsAcrossClauses")>]
        alignItemsAcrossClauses: bool
        /// Indent list items.
        [<JsonPropertyName("indentListItems")>]
        indentListItems: bool
        /// Align list items to tab stops.
        [<JsonPropertyName("alignItemsToTabStops")>]
        alignItemsToTabStops: bool
        /// Align aliases to each other.
        [<JsonPropertyName("alignAliases")>]
        alignAliases: bool
        /// Align comments to each other.
        [<JsonPropertyName("alignComments")>]
        alignComments: bool
        /// Place commas before list items.
        [<JsonPropertyName("placeCommasBeforeItems")>]
        placeCommasBeforeItems: bool
        /// Add space before commas.
        [<JsonPropertyName("addSpaceBeforeComma")>]
        addSpaceBeforeComma: bool
        /// Add space after commas.
        [<JsonPropertyName("addSpaceAfterComma")>]
        addSpaceAfterComma: bool
        /// How commas separating list items are aligned.
        [<JsonPropertyName("commaAlignment")>]
        commaAlignment: CommaAlignment
    }

/// Formatting options for parentheses.
type Parentheses =
    {
        /// The format to use for parenthesized code.
        [<JsonPropertyName("parenthesisStyle")>]
        parenthesisStyle: ParenthesisStyle
        /// Whether the contents of parentheses are indented.
        [<JsonPropertyName("indentParenthesesContents")>]
        indentParenthesesContents: bool
        /// Collapse short contents of parentheses onto a single line.
        [<JsonPropertyName("collapseShortParenthesisContents")>]
        collapseShortParenthesisContents: bool
        /// Contents with fewer than this number of characters will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseParenthesesShorterThan")>]
        collapseParenthesesShorterThan: int
        /// Whether spaces will be added around parentheses.
        [<JsonPropertyName("addSpacesAroundParentheses")>]
        addSpacesAroundParentheses: bool
        /// Whether spaces are added around parentheses' contents.
        [<JsonPropertyName("addSpacesInsideParentheses")>]
        addSpacesInsideParentheses: bool
    }

/// Formatting options for casing.
type Casing =
    {
        /// How reserved keywords are cased.
        [<JsonPropertyName("reservedKeywords")>]
        reservedKeywords: CasingStyle
        /// How built-in functions are cased.
        [<JsonPropertyName("builtInFunctions")>]
        builtInFunctions: CasingStyle
        /// How built-in data types are cased.
        [<JsonPropertyName("builtInDataTypes")>]
        builtInDataTypes: CasingStyle
        /// How global variables are cased.
        [<JsonPropertyName("globalVariables")>]
        globalVariables: CasingStyle
        /// Use the case from the definition when casing objects.
        [<JsonPropertyName("useObjectDefinitionCase")>]
        useObjectDefinitionCase: bool
    }

/// Formatting options for DML clauses.
type DmlClauses =
    {
        /// How clauses withing DML statements are aligned.
        [<JsonPropertyName("clauseAlignment")>]
        clauseAlignment: ClauseAlignment
        /// The number of spaces to indent clauses within DML statements.
        [<JsonPropertyName("clauseIndentation")>]
        clauseIndentation: int
    }

/// Formatting options for DML list items.
type DmlListItems =
    {
        /// When to place the table in a DML statement on a new line.
        [<JsonPropertyName("placeFromTableOnNewLine")>]
        placeFromTableOnNewLine: PlaceOnNewLine
        /// When to place a WHERE condition in a DML statement on a new line.
        [<JsonPropertyName("placeWhereConditionOnNewLine")>]
        placeWhereConditionOnNewLine: PlaceOnNewLine
        /// When to place GROUP BY and ORDER BY clauses in a DML statement on a new line.
        [<JsonPropertyName("placeGroupByAndOrderByOnNewLine")>]
        placeGroupByAndOrderByOnNewLine: PlaceOnNewLine
    }

/// Formatting options for data (DML).
type Dml =
    {
        /// In INSERT table statements, place table name on a new line.
        [<JsonPropertyName("placeInsertTableOnNewLine")>]
        placeInsertTableOnNewLine: bool
        /// Whether to place DISTINCT and TOP clauses on a new line.
        [<JsonPropertyName("placeDistinctAndTopClausesOnNewLine")>]
        placeDistinctAndTopClausesOnNewLine: bool
        /// Whether to add a new line after DISTINCT and TOP clauses.
        [<JsonPropertyName("addNewLineAfterDistinctAndTopClauses")>]
        addNewLineAfterDistinctAndTopClauses: bool
        /// Collapse short statements onto a single line.
        [<JsonPropertyName("collapseShortStatements")>]
        collapseShortStatements: bool
        /// Statements that are shorter than this will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseStatementsShorterThan")>]
        collapseStatementsShorterThan: int
        /// Collapse short subqueries onto a single line.
        [<JsonPropertyName("collapseShortSubqueries")>]
        collapseShortSubqueries: bool
        /// Subqueries that are shorter than this will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseSubqueriesShorterThan")>]
        collapseSubqueriesShorterThan: int
        /// Formatting options for DML clauses.
        [<JsonPropertyName("clauses")>]
        clauses: DmlClauses
        /// Formatting options for DML list items.
        [<JsonPropertyName("listItems")>]
        listItems: DmlListItems
    }

/// Formatting options for schema (DDL).
type Ddl =
    {
        /// The format to use for parenthesized DDL code.
        [<JsonPropertyName("parenthesisStyle")>]
        parenthesisStyle: ParenthesisStyle
        /// Whether the contents of parentheses in DDL statements are indented.
        [<JsonPropertyName("indentParenthesesContents")>]
        indentParenthesesContents: bool
        /// Whether to align data types and constraints.
        [<JsonPropertyName("alignDataTypesAndConstraints")>]
        alignDataTypesAndConstraints: bool
        /// Whether to place constraints on a new line.
        [<JsonPropertyName("placeConstraintsOnNewLines")>]
        placeConstraintsOnNewLines: bool
        /// When to place constraint columns on a new line.
        [<JsonPropertyName("placeConstraintColumnsOnNewLines")>]
        placeConstraintColumnsOnNewLines: PlaceOnNewLine
        /// Whether to indent clauses in DDL statements.
        [<JsonPropertyName("indentClauses")>]
        indentClauses: bool
        /// Whether to place the first parameter of a stored procedure on a new line.
        [<JsonPropertyName("placeFirstProcedureParameterOnNewLine")>]
        placeFirstProcedureParameterOnNewLine: PlaceOnNewLine
        /// Collapse short DDL statements onto a single line.
        [<JsonPropertyName("collapseShortStatements")>]
        collapseShortStatements: bool
        /// DDL statements that are shorter than this will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseStatementsShorterThan")>]
        collapseStatementsShorterThan: int
    }

/// Formatting options for control flow.
type ControlFlow =
    {
        /// Whether BEGIN and END keywords are placed on new lines.
        [<JsonPropertyName("placeBeginAndEndOnNewLine")>]
        placeBeginAndEndOnNewLine: bool
        /// Whether to indent BEGIN and END keywords.
        [<JsonPropertyName("indentBeginAndEndKeywords")>]
        indentBeginAndEndKeywords: bool
        /// Whether the contents of control flow statements are indented.
        [<JsonPropertyName("indentContentsOfStatements")>]
        indentContentsOfStatements: bool
        /// Collapse short control flow statements onto a single line.
        [<JsonPropertyName("collapseShortStatements")>]
        collapseShortStatements: bool
        /// Control flow statements that are shorter than this will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseStatementsShorterThan")>]
        collapseStatementsShorterThan: int
    }

/// Formatting options for CTEs.
type Cte =
    {
        /// The format to use for parenthesized CTE code.
        [<JsonPropertyName("parenthesisStyle")>]
        parenthesisStyle: ParenthesisStyle
        /// Whether to indent clauses in CTE statements.
        [<JsonPropertyName("indentContents")>]
        indentContents: bool
        /// Whether the CTE name is placed on a new line.
        [<JsonPropertyName("placeNameOnNewLine")>]
        placeNameOnNewLine: bool
        /// Whether the CTE name is indented.
        [<JsonPropertyName("indentName")>]
        indentName: bool
        /// Whether the CTE columns are placed on a new line.
        [<JsonPropertyName("placeColumnsOnNewLine")>]
        placeColumnsOnNewLine: bool
        /// How the columns in a CTE are aligned.
        [<JsonPropertyName("columnAlignment")>]
        columnAlignment: Alignment
        /// Whether the AS keyword is placed on a new line.
        [<JsonPropertyName("placeAsOnNewLine")>]
        placeAsOnNewLine: bool
        /// How the AS keyword is aligned.
        [<JsonPropertyName("asAlignment")>]
        asAlignment: Alignment
    }

/// Formatting options for variables.
type Variables =
    {
        /// Whether to align the data types and values in DECLARE statements.
        [<JsonPropertyName("alignDataTypesAndValues")>]
        alignDataTypesAndValues: bool
        /// Whether a space is added between a data type and its precision.
        [<JsonPropertyName("addSpaceBetweenDataTypeAndPrecision")>]
        addSpaceBetweenDataTypeAndPrecision: bool
        /// Whether the assigned value is placed on a new line in long SET statements.
        [<JsonPropertyName("placeAssignedValueOnNewLineIfLongerThanMaxLineLength")>]
        placeAssignedValueOnNewLineIfLongerThanMaxLineLength: bool
        /// Whether the = sign is placed on a new line.
        [<JsonPropertyName("placeEqualsSignOnNewLine")>]
        placeEqualsSignOnNewLine: bool
    }

/// Formatting options for JOIN.
type Join =
    {
        /// Whether the JOIN keyword is placed on a new line.
        [<JsonPropertyName("placeOnNewLine")>]
        placeOnNewLine: bool
        /// How the JOIN keyword is aligned.
        [<JsonPropertyName("keywordAlignment")>]
        keywordAlignment: JoinKeywordAlignment
        /// Whether empty lines are inserted between JOIN clauses.
        [<JsonPropertyName("insertEmptyLineBetweenJoinClauses")>]
        insertEmptyLineBetweenJoinClauses: bool
        /// Whether the join table is placed on a new line.
        [<JsonPropertyName("placeJoinTableOnNewLine")>]
        placeJoinTableOnNewLine: bool
        /// Whether the join table is indented.
        [<JsonPropertyName("indentJoinTable")>]
        indentJoinTable: bool
    }

/// Formatting options for ON.
type On =
    {
        /// Whether the ON keyword is placed on a new line.
        [<JsonPropertyName("placeOnNewLine")>]
        placeOnNewLine: bool
        /// How the ON keyword is aligned.
        [<JsonPropertyName("keywordAlignment")>]
        keywordAlignment: OnKeywordAlignment
        /// Whether a join condition is placed on a new line.
        [<JsonPropertyName("placeConditionOnNewLine")>]
        placeConditionOnNewLine: bool
        /// How join conditions are aligned.
        [<JsonPropertyName("conditionAlignment")>]
        conditionAlignment: Alignment
    }

/// Formatting options for join statements.
type JoinStatements =
    {
        /// Formatting options for JOIN.
        [<JsonPropertyName("join")>]
        join: Join
        /// Formatting options for ON.
        [<JsonPropertyName("on")>]
        on: On
    }

/// Formatting options for columns.
type InsertColumns =
    {
        /// The format to use for parenthesized insert statements.
        [<JsonPropertyName("parenthesisStyle")>]
        parenthesisStyle: ParenthesisStyle
        /// Whether to indent content.
        [<JsonPropertyName("indentContents")>]
        indentContents: bool
        /// When to place subsequent columns on new lines.
        [<JsonPropertyName("placeSubsequentColumnsOnNewLines")>]
        placeSubsequentColumnsOnNewLines: PlaceOnNewLine
    }

/// Formatting options for values.
type InsertValues =
    {
        /// The format to use for values in INSERT statements.
        [<JsonPropertyName("parenthesisStyle")>]
        parenthesisStyle: ParenthesisStyle
        /// Whether to indent content.
        [<JsonPropertyName("indentContents")>]
        indentContents: bool
        /// When to place subsequent values on new lines.
        [<JsonPropertyName("placeSubsequentValuesOnNewLines")>]
        placeSubsequentValuesOnNewLines: PlaceOnNewLine
    }

/// Formatting options for inserts.
type InsertStatements =
    {
        /// Formatting options for columns.
        [<JsonPropertyName("columns")>]
        columns: InsertColumns
        /// Formatting options for values.
        [<JsonPropertyName("values")>]
        values: InsertValues
    }

/// Formatting options for function calls.
type FunctionCalls =
    {
        /// When to place arguments on new lines.
        [<JsonPropertyName("placeArgumentsOnNewLines")>]
        placeArgumentsOnNewLines: PlaceOnNewLine
        /// Whether to add spaces around parentheses.
        [<JsonPropertyName("addSpacesAroundParentheses")>]
        addSpacesAroundParentheses: bool
        /// Whether to add spaces around argument list.
        [<JsonPropertyName("addSpacesAroundArgumentList")>]
        addSpacesAroundArgumentList: bool
        /// Whether to add space between empty parentheses.
        [<JsonPropertyName("addSpaceBetweenEmptyParentheses")>]
        addSpaceBetweenEmptyParentheses: bool
    }

/// Formatting options for CASE.
type CaseExpressions =
    {
        /// Whether to place expression on new line.
        [<JsonPropertyName("placeExpressionOnNewLine")>]
        placeExpressionOnNewLine: bool
        /// When to place the first WHEN keyword on a new line.
        [<JsonPropertyName("placeFirstWhenOnNewLine")>]
        placeFirstWhenOnNewLine: PlaceOnNewLine
        /// How the WHEN keywords are aligned.
        [<JsonPropertyName("whenAlignment")>]
        whenAlignment: WhenAlignment
        /// Whether the THEN keyword is placed on a new line.
        [<JsonPropertyName("placeThenOnNewLine")>]
        placeThenOnNewLine: bool
        /// How the THEN keywords are aligned.
        [<JsonPropertyName("thenAlignment")>]
        thenAlignment: Alignment
        /// Whether the ELSE keyword is placed on new line.
        [<JsonPropertyName("placeElseOnNewLine")>]
        placeElseOnNewLine: bool
        /// Whether to align the ELSE keyword to the WHEN keyword.
        [<JsonPropertyName("alignElseToWhen")>]
        alignElseToWhen: bool
        /// Whether to place the END keyword on new line.
        [<JsonPropertyName("placeEndOnNewLine")>]
        placeEndOnNewLine: bool
        /// How the END keyword is aligned.
        [<JsonPropertyName("endAlignment")>]
        endAlignment: EndAlignment
        /// Collapse short CASE expressions onto a single line.
        [<JsonPropertyName("collapseShortCaseExpressions")>]
        collapseShortCaseExpressions: bool
        /// CASE expressions shorter than this will be collapsed onto a single line if enabled.
        [<JsonPropertyName("collapseCaseExpressionsShorterThan")>]
        collapseCaseExpressionsShorterThan: int
    }

/// Formatting options for comparisons.
type Comparison =
    {
        /// Whether to align comparison operators.
        [<JsonPropertyName("align")>]
        align: bool
        /// Whether to add spaces around comparison operators.
        [<JsonPropertyName("addSpacesAround")>]
        addSpacesAround: bool
    }

/// Formatting options for arithmetic.
type Arithmetic =
    {
        /// Whether to add spaces around arithmetic operators.
        [<JsonPropertyName("addSpacesAround")>]
        addSpacesAround: bool
    }

/// Formatting options for AND / OR.
type AndOr =
    {
        /// When to place AND / OR operators on a new line.
        [<JsonPropertyName("placeOnNewLine")>]
        placeOnNewLine: PlaceOnNewLine
        /// How to align AND / OR operators.
        [<JsonPropertyName("alignment")>]
        alignment: Alignment
        /// Whether to place AND / OR keyword before condition.
        [<JsonPropertyName("placeKeywordBeforeCondition")>]
        placeKeywordBeforeCondition: bool
    }

/// Formatting options for BETWEEN.
type Between =
    {
        /// Whether to place BETWEEN operator on a new line.
        [<JsonPropertyName("placeOnNewLine")>]
        placeOnNewLine: bool
        /// Whether to place AND keyword on a new line.
        [<JsonPropertyName("placeAndKeywordOnNewLine")>]
        placeAndKeywordOnNewLine: bool
        /// How to align the AND keyword if placing AND keyword on a new line is enabled.
        [<JsonPropertyName("andAlignment")>]
        andAlignment: Alignment
    }

/// Formatting options for IN.
type ``In`` =
    {
        /// Whether to place opening parenthesis on new line.
        [<JsonPropertyName("placeOpeningParenthesisOnNewLine")>]
        placeOpeningParenthesisOnNewLine: bool
        /// How to align the IN keyword.
        [<JsonPropertyName("alignment")>]
        alignment: InAlignment
        /// When to place the first value on a new line.
        [<JsonPropertyName("placeFirstValueOnNewLine")>]
        placeFirstValueOnNewLine: PlaceOnNewLine
        /// When to place subsequent values on new lines.
        [<JsonPropertyName("placeSubsequentValuesOnNewLines")>]
        placeSubsequentValuesOnNewLines: PlaceOnNewLine
        /// Whether to add a space around IN contents.
        [<JsonPropertyName("addSpaceAroundInContents")>]
        addSpaceAroundInContents: bool
    }

/// Formatting options for operators.
type Operators =
    {
        /// Formatting options for comparisons.
        [<JsonPropertyName("comparison")>]
        comparison: Comparison
        /// Formatting options for arithmetic.
        [<JsonPropertyName("arithmetic")>]
        arithmetic: Arithmetic
        /// Formatting options for AND / OR.
        [<JsonPropertyName("andOr")>]
        andOr: AndOr
        /// Formatting options for BETWEEN.
        [<JsonPropertyName("between")>]
        between: Between
        /// Formatting options for IN.
        [<JsonPropertyName("in")>]
        ``in``: ``In``
    }

/// Custom set-operator formatting options.
type SetOperationsExtensions =
    {
        /// Insert a blank line before and after UNION/EXCEPT/INTERSECT operators.
        [<JsonPropertyName("blankLinesAroundOperators")>]
        blankLinesAroundOperators: bool
    }

/// Custom CTE formatting options.
type CteExtensions =
    {
        /// Omit the leading semicolon before CTEs (emit WITH instead of ;WITH).
        [<JsonPropertyName("omitLeadingSemicolon")>]
        omitLeadingSemicolon: bool
    }

/// Custom options used by tsqlfmt (not part of SQL Prompt schema).
type FormatterExtensions =
    {
        /// Custom CTE formatting options.
        [<JsonPropertyName("cte")>]
        cte: CteExtensions
        /// Custom set-operator formatting options.
        [<JsonPropertyName("setOperations")>]
        setOperations: SetOperationsExtensions
    }

/// Root formatting style configuration.
type Style =
    {
        /// Formatting options for whitespace.
        [<JsonPropertyName("whitespace")>]
        whitespace: Whitespace
        /// Formatting options for lists.
        [<JsonPropertyName("lists")>]
        lists: Lists
        /// Formatting options for parentheses.
        [<JsonPropertyName("parentheses")>]
        parentheses: Parentheses
        /// Formatting options for casing.
        [<JsonPropertyName("casing")>]
        casing: Casing
        /// Formatting options for data (DML).
        [<JsonPropertyName("dml")>]
        dml: Dml
        /// Formatting options for schema (DDL).
        [<JsonPropertyName("ddl")>]
        ddl: Ddl
        /// Formatting options for control flow.
        [<JsonPropertyName("controlFlow")>]
        controlFlow: ControlFlow
        /// Formatting options for CTEs.
        [<JsonPropertyName("cte")>]
        cte: Cte
        /// Formatting options for variables.
        [<JsonPropertyName("variables")>]
        variables: Variables
        /// Formatting options for join statements.
        [<JsonPropertyName("joinStatements")>]
        joinStatements: JoinStatements
        /// Formatting options for inserts.
        [<JsonPropertyName("insertStatements")>]
        insertStatements: InsertStatements
        /// Formatting options for function calls.
        [<JsonPropertyName("functionCalls")>]
        functionCalls: FunctionCalls
        /// Formatting options for CASE.
        [<JsonPropertyName("caseExpressions")>]
        caseExpressions: CaseExpressions
        /// Formatting options for operators.
        [<JsonPropertyName("operators")>]
        operators: Operators
        /// Custom options used by tsqlfmt (not part of SQL Prompt schema).
        [<JsonPropertyName("formatterExtensions")>]
        formatterExtensions: FormatterExtensions
    }

let private unsupportedRightAlignmentMessage settingName valueName =
    sprintf
        "%s = %s is not supported. tsqlfmt supports structural indentation, but not right-aligned layouts."
        settingName
        valueName

let validateStyle (style: Style) : Style =
    let fail settingName valueName =
        invalidArg settingName (unsupportedRightAlignmentMessage settingName valueName)

    match style.dml.clauses.clauseAlignment with
    | ClauseAlignment.RightAligned -> fail "dml.clauses.clauseAlignment" "rightAligned"
    | _ -> ()

    match style.cte.asAlignment with
    | Alignment.RightAligned -> fail "cte.asAlignment" "rightAligned"
    | _ -> ()

    match style.joinStatements.join.keywordAlignment with
    | JoinKeywordAlignment.RightAlignedToFrom -> fail "joinStatements.join.keywordAlignment" "rightAlignedToFrom"
    | _ -> ()

    match style.joinStatements.on.keywordAlignment with
    | OnKeywordAlignment.RightAlignedToJoin -> fail "joinStatements.on.keywordAlignment" "rightAlignedToJoin"
    | OnKeywordAlignment.RightAlignedToInner -> fail "joinStatements.on.keywordAlignment" "rightAlignedToInner"
    | _ -> ()

    match style.caseExpressions.endAlignment with
    | EndAlignment.RightAlignedToWhen -> fail "caseExpressions.endAlignment" "rightAlignedToWhen"
    | _ -> ()

    match style.operators.andOr.alignment with
    | Alignment.RightAligned -> fail "operators.andOr.alignment" "rightAligned"
    | _ -> ()

    match style.operators.between.andAlignment with
    | Alignment.RightAligned -> fail "operators.between.andAlignment" "rightAligned"
    | _ -> ()

    match style.operators.``in``.alignment with
    | InAlignment.RightAligned -> fail "operators.in.alignment" "rightAligned"
    | _ -> ()

    style

// ─── Defaults ───

let defaultNewLines =
    { preserveExistingEmptyLinesBetweenStatements = true
      preserveExistingEmptyLinesAfterBatchSeparator = true
      emptyLinesBetweenStatements = 1
      emptyLinesAfterBatchSeparator = 1 }

let defaultWhitespace =
    { spacesOrTabs = SpacesOrTabs.Spaces
      numberOfSpacesInTabs = 4
      wrapLongLines = true
      wrapLinesLongerThan = 120
      whiteSpaceBeforeSemiColon = WhiteSpaceBeforeSemiColon.None
      newLines = defaultNewLines }

let defaultLists =
    { placeFirstItemOnNewLine = PlaceOnNewLine.Never
      placeSubsequentItemsOnNewLines = PlaceOnNewLine.Always
      alignSubsequentItemsWithFirstItem = true
      alignItemsAcrossClauses = true
      indentListItems = true
      alignItemsToTabStops = false
      alignAliases = false
      alignComments = false
      placeCommasBeforeItems = false
      addSpaceBeforeComma = false
      addSpaceAfterComma = true
      commaAlignment = CommaAlignment.ToList }

let defaultParentheses =
    { parenthesisStyle = ParenthesisStyle.CompactSimple
      indentParenthesesContents = false
      collapseShortParenthesisContents = false
      collapseParenthesesShorterThan = 80
      addSpacesAroundParentheses = true
      addSpacesInsideParentheses = false }

let defaultCasing =
    { reservedKeywords = CasingStyle.LeaveAsIs
      builtInFunctions = CasingStyle.LeaveAsIs
      builtInDataTypes = CasingStyle.LeaveAsIs
      globalVariables = CasingStyle.LeaveAsIs
      useObjectDefinitionCase = false }

let defaultDmlClauses =
    { clauseAlignment = ClauseAlignment.LeftAligned
      clauseIndentation = 0 }

let defaultDmlListItems =
    { placeFromTableOnNewLine = PlaceOnNewLine.Never
      placeWhereConditionOnNewLine = PlaceOnNewLine.Never
      placeGroupByAndOrderByOnNewLine = PlaceOnNewLine.Never }

let defaultDml =
    { placeInsertTableOnNewLine = false
      placeDistinctAndTopClausesOnNewLine = false
      addNewLineAfterDistinctAndTopClauses = false
      collapseShortStatements = false
      collapseStatementsShorterThan = 80
      collapseShortSubqueries = false
      collapseSubqueriesShorterThan = 80
      clauses = defaultDmlClauses
      listItems = defaultDmlListItems }

let defaultDdl =
    { parenthesisStyle = ParenthesisStyle.CompactSimple
      indentParenthesesContents = false
      alignDataTypesAndConstraints = true
      placeConstraintsOnNewLines = false
      placeConstraintColumnsOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
      indentClauses = false
      placeFirstProcedureParameterOnNewLine = PlaceOnNewLine.IfMultipleItems
      collapseShortStatements = false
      collapseStatementsShorterThan = 80 }

let defaultControlFlow =
    { placeBeginAndEndOnNewLine = true
      indentBeginAndEndKeywords = false
      indentContentsOfStatements = true
      collapseShortStatements = false
      collapseStatementsShorterThan = 80 }

let defaultCte =
    { parenthesisStyle = ParenthesisStyle.CompactSimple
      indentContents = false
      placeNameOnNewLine = false
      indentName = false
      placeColumnsOnNewLine = false
      columnAlignment = Alignment.LeftAligned
      placeAsOnNewLine = true
      asAlignment = Alignment.LeftAligned }

let defaultVariables =
    { alignDataTypesAndValues = true
      addSpaceBetweenDataTypeAndPrecision = false
      placeAssignedValueOnNewLineIfLongerThanMaxLineLength = true
      placeEqualsSignOnNewLine = false }

let defaultJoin =
    { placeOnNewLine = true
      keywordAlignment = JoinKeywordAlignment.ToFrom
      insertEmptyLineBetweenJoinClauses = false
      placeJoinTableOnNewLine = false
      indentJoinTable = true }

let defaultOn =
    { placeOnNewLine = true
      keywordAlignment = OnKeywordAlignment.ToJoin
      placeConditionOnNewLine = false
      conditionAlignment = Alignment.LeftAligned }

let defaultJoinStatements = { join = defaultJoin; on = defaultOn }

let defaultInsertColumns =
    { parenthesisStyle = ParenthesisStyle.ExpandedToStatement
      indentContents = true
      placeSubsequentColumnsOnNewLines = PlaceOnNewLine.Always }

let defaultInsertValues =
    { parenthesisStyle = ParenthesisStyle.CompactToStatement
      indentContents = false
      placeSubsequentValuesOnNewLines = PlaceOnNewLine.Never }

let defaultInsertStatements =
    { columns = defaultInsertColumns
      values = defaultInsertValues }

let defaultFunctionCalls =
    { placeArgumentsOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
      addSpacesAroundParentheses = false
      addSpacesAroundArgumentList = false
      addSpaceBetweenEmptyParentheses = false }

let defaultCaseExpressions =
    { placeExpressionOnNewLine = true
      placeFirstWhenOnNewLine = PlaceOnNewLine.Always
      whenAlignment = WhenAlignment.IndentedFromCase
      placeThenOnNewLine = false
      thenAlignment = Alignment.Indented
      placeElseOnNewLine = true
      alignElseToWhen = true
      placeEndOnNewLine = true
      endAlignment = EndAlignment.ToCase
      collapseShortCaseExpressions = false
      collapseCaseExpressionsShorterThan = 80 }

let defaultComparison =
    { align = false
      addSpacesAround = true }

let defaultArithmetic = { addSpacesAround = true }

let defaultAndOr =
    { placeOnNewLine = PlaceOnNewLine.Always
      alignment = Alignment.LeftAligned
      placeKeywordBeforeCondition = true }

let defaultBetween =
    { placeOnNewLine = true
      placeAndKeywordOnNewLine = false
      andAlignment = Alignment.LeftAligned }

let defaultIn =
    { placeOpeningParenthesisOnNewLine = false
      alignment = InAlignment.LeftAligned
      placeFirstValueOnNewLine = PlaceOnNewLine.IfLongerThanMaxLineLength
      placeSubsequentValuesOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
      addSpaceAroundInContents = false }

let defaultOperators =
    { comparison = defaultComparison
      arithmetic = defaultArithmetic
      andOr = defaultAndOr
      between = defaultBetween
      ``in`` = defaultIn }

let defaultSetOperationsExtensions = { blankLinesAroundOperators = true }

let defaultCteExtensions = { omitLeadingSemicolon = false }

let defaultFormatterExtensions =
    { cte = defaultCteExtensions
      setOperations = defaultSetOperationsExtensions }

let defaultStyle: Style =
    { whitespace = defaultWhitespace
      lists = defaultLists
      parentheses = defaultParentheses
      casing = defaultCasing
      dml = defaultDml
      ddl = defaultDdl
      controlFlow = defaultControlFlow
      cte = defaultCte
      variables = defaultVariables
      joinStatements = defaultJoinStatements
      insertStatements = defaultInsertStatements
      functionCalls = defaultFunctionCalls
      caseExpressions = defaultCaseExpressions
      operators = defaultOperators
      formatterExtensions = defaultFormatterExtensions }

// ─── JSON Loading ───

let private getJsonOpt (el: JsonElement) (name: string) =
    match el.TryGetProperty(name) with
    | true, v -> Some v
    | _ -> None

let private getJsonBool (el: JsonElement) (name: string) (def: bool) =
    match getJsonOpt el name with
    | Some v -> v.GetBoolean()
    | None -> def

let private getJsonInt (el: JsonElement) (name: string) (def: int) =
    match getJsonOpt el name with
    | Some v -> v.GetInt32()
    | None -> def

let private getJsonEnum<'T when 'T :> System.Enum and 'T: struct> (el: JsonElement) (name: string) (def: 'T) : 'T =
    match getJsonOpt el name with
    | Some v ->
        let s = v.GetString()
        let opts = JsonSerializerOptions()
        opts.Converters.Add(JsonStringEnumConverter())
        JsonSerializer.Deserialize<'T>("\"" + s + "\"", opts)
    | None -> def

/// Merge a partially-specified JSON config over the defaults.
/// This parses the full SQL Prompt-style config shape first.
/// Validation of unsupported layout modes is a separate step.
let loadStyle (path: string) : Style =
    let json = File.ReadAllText(path)

    // We need to use JsonDocument to merge partial JSON over defaults
    let doc = JsonDocument.Parse(json)
    let root = doc.RootElement

    let getOpt = getJsonOpt
    let getBool = getJsonBool
    let getInt = getJsonInt
    let getEnum el name def = getJsonEnum el name def

    // Parse each section
    let whitespace =
        match getOpt root "whitespace" with
        | Some ws ->
            let nl =
                match getOpt ws "newLines" with
                | Some n ->
                    { preserveExistingEmptyLinesBetweenStatements =
                        getBool
                            n
                            "preserveExistingEmptyLinesBetweenStatements"
                            defaultNewLines.preserveExistingEmptyLinesBetweenStatements
                      preserveExistingEmptyLinesAfterBatchSeparator =
                        getBool
                            n
                            "preserveExistingEmptyLinesAfterBatchSeparator"
                            defaultNewLines.preserveExistingEmptyLinesAfterBatchSeparator
                      emptyLinesBetweenStatements =
                        getInt n "emptyLinesBetweenStatements" defaultNewLines.emptyLinesBetweenStatements
                      emptyLinesAfterBatchSeparator =
                        getInt n "emptyLinesAfterBatchSeparator" defaultNewLines.emptyLinesAfterBatchSeparator }
                | None -> defaultNewLines

            { spacesOrTabs = getEnum ws "spacesOrTabs" defaultWhitespace.spacesOrTabs
              numberOfSpacesInTabs = getInt ws "numberOfSpacesInTabs" defaultWhitespace.numberOfSpacesInTabs
              wrapLongLines = getBool ws "wrapLongLines" defaultWhitespace.wrapLongLines
              wrapLinesLongerThan = getInt ws "wrapLinesLongerThan" defaultWhitespace.wrapLinesLongerThan
              whiteSpaceBeforeSemiColon =
                getEnum ws "whiteSpaceBeforeSemiColon" defaultWhitespace.whiteSpaceBeforeSemiColon
              newLines = nl }
        | None -> defaultWhitespace

    let lists =
        match getOpt root "lists" with
        | Some ls ->
            { placeFirstItemOnNewLine = getEnum ls "placeFirstItemOnNewLine" defaultLists.placeFirstItemOnNewLine
              placeSubsequentItemsOnNewLines =
                getEnum ls "placeSubsequentItemsOnNewLines" defaultLists.placeSubsequentItemsOnNewLines
              alignSubsequentItemsWithFirstItem =
                getBool ls "alignSubsequentItemsWithFirstItem" defaultLists.alignSubsequentItemsWithFirstItem
              alignItemsAcrossClauses = getBool ls "alignItemsAcrossClauses" defaultLists.alignItemsAcrossClauses
              indentListItems = getBool ls "indentListItems" defaultLists.indentListItems
              alignItemsToTabStops = getBool ls "alignItemsToTabStops" defaultLists.alignItemsToTabStops
              alignAliases = getBool ls "alignAliases" defaultLists.alignAliases
              alignComments = getBool ls "alignComments" defaultLists.alignComments
              placeCommasBeforeItems = getBool ls "placeCommasBeforeItems" defaultLists.placeCommasBeforeItems
              addSpaceBeforeComma = getBool ls "addSpaceBeforeComma" defaultLists.addSpaceBeforeComma
              addSpaceAfterComma = getBool ls "addSpaceAfterComma" defaultLists.addSpaceAfterComma
              commaAlignment = getEnum ls "commaAlignment" defaultLists.commaAlignment }
        | None -> defaultLists

    let parentheses =
        match getOpt root "parentheses" with
        | Some p ->
            { parenthesisStyle = getEnum p "parenthesisStyle" defaultParentheses.parenthesisStyle
              indentParenthesesContents =
                getBool p "indentParenthesesContents" defaultParentheses.indentParenthesesContents
              collapseShortParenthesisContents =
                getBool p "collapseShortParenthesisContents" defaultParentheses.collapseShortParenthesisContents
              collapseParenthesesShorterThan =
                getInt p "collapseParenthesesShorterThan" defaultParentheses.collapseParenthesesShorterThan
              addSpacesAroundParentheses =
                getBool p "addSpacesAroundParentheses" defaultParentheses.addSpacesAroundParentheses
              addSpacesInsideParentheses =
                getBool p "addSpacesInsideParentheses" defaultParentheses.addSpacesInsideParentheses }
        | None -> defaultParentheses

    let casing =
        match getOpt root "casing" with
        | Some c ->
            { reservedKeywords = getEnum c "reservedKeywords" defaultCasing.reservedKeywords
              builtInFunctions = getEnum c "builtInFunctions" defaultCasing.builtInFunctions
              builtInDataTypes = getEnum c "builtInDataTypes" defaultCasing.builtInDataTypes
              globalVariables = getEnum c "globalVariables" defaultCasing.globalVariables
              useObjectDefinitionCase = getBool c "useObjectDefinitionCase" defaultCasing.useObjectDefinitionCase }
        | None -> defaultCasing

    let dml =
        match getOpt root "dml" with
        | Some d ->
            let clauses =
                match getOpt d "clauses" with
                | Some c ->
                    { clauseAlignment = getEnum c "clauseAlignment" defaultDmlClauses.clauseAlignment
                      clauseIndentation = getInt c "clauseIndentation" defaultDmlClauses.clauseIndentation }
                | None -> defaultDmlClauses

            let listItems =
                match getOpt d "listItems" with
                | Some li ->
                    { placeFromTableOnNewLine =
                        getEnum li "placeFromTableOnNewLine" defaultDmlListItems.placeFromTableOnNewLine
                      placeWhereConditionOnNewLine =
                        getEnum li "placeWhereConditionOnNewLine" defaultDmlListItems.placeWhereConditionOnNewLine
                      placeGroupByAndOrderByOnNewLine =
                        getEnum li "placeGroupByAndOrderByOnNewLine" defaultDmlListItems.placeGroupByAndOrderByOnNewLine }
                | None -> defaultDmlListItems

            { placeInsertTableOnNewLine = getBool d "placeInsertTableOnNewLine" defaultDml.placeInsertTableOnNewLine
              placeDistinctAndTopClausesOnNewLine =
                getBool d "placeDistinctAndTopClausesOnNewLine" defaultDml.placeDistinctAndTopClausesOnNewLine
              addNewLineAfterDistinctAndTopClauses =
                getBool d "addNewLineAfterDistinctAndTopClauses" defaultDml.addNewLineAfterDistinctAndTopClauses
              collapseShortStatements = getBool d "collapseShortStatements" defaultDml.collapseShortStatements
              collapseStatementsShorterThan =
                getInt d "collapseStatementsShorterThan" defaultDml.collapseStatementsShorterThan
              collapseShortSubqueries = getBool d "collapseShortSubqueries" defaultDml.collapseShortSubqueries
              collapseSubqueriesShorterThan =
                getInt d "collapseSubqueriesShorterThan" defaultDml.collapseSubqueriesShorterThan
              clauses = clauses
              listItems = listItems }
        | None -> defaultDml

    let ddl =
        match getOpt root "ddl" with
        | Some d ->
            { parenthesisStyle = getEnum d "parenthesisStyle" defaultDdl.parenthesisStyle
              indentParenthesesContents = getBool d "indentParenthesesContents" defaultDdl.indentParenthesesContents
              alignDataTypesAndConstraints =
                getBool d "alignDataTypesAndConstraints" defaultDdl.alignDataTypesAndConstraints
              placeConstraintsOnNewLines = getBool d "placeConstraintsOnNewLines" defaultDdl.placeConstraintsOnNewLines
              placeConstraintColumnsOnNewLines =
                getEnum d "placeConstraintColumnsOnNewLines" defaultDdl.placeConstraintColumnsOnNewLines
              indentClauses = getBool d "indentClauses" defaultDdl.indentClauses
              placeFirstProcedureParameterOnNewLine =
                getEnum d "placeFirstProcedureParameterOnNewLine" defaultDdl.placeFirstProcedureParameterOnNewLine
              collapseShortStatements = getBool d "collapseShortStatements" defaultDdl.collapseShortStatements
              collapseStatementsShorterThan =
                getInt d "collapseStatementsShorterThan" defaultDdl.collapseStatementsShorterThan }
        | None -> defaultDdl

    let controlFlow =
        match getOpt root "controlFlow" with
        | Some cf ->
            { placeBeginAndEndOnNewLine =
                getBool cf "placeBeginAndEndOnNewLine" defaultControlFlow.placeBeginAndEndOnNewLine
              indentBeginAndEndKeywords =
                getBool cf "indentBeginAndEndKeywords" defaultControlFlow.indentBeginAndEndKeywords
              indentContentsOfStatements =
                getBool cf "indentContentsOfStatements" defaultControlFlow.indentContentsOfStatements
              collapseShortStatements = getBool cf "collapseShortStatements" defaultControlFlow.collapseShortStatements
              collapseStatementsShorterThan =
                getInt cf "collapseStatementsShorterThan" defaultControlFlow.collapseStatementsShorterThan }
        | None -> defaultControlFlow

    let cte =
        match getOpt root "cte" with
        | Some c ->
            { parenthesisStyle = getEnum c "parenthesisStyle" defaultCte.parenthesisStyle
              indentContents = getBool c "indentContents" defaultCte.indentContents
              placeNameOnNewLine = getBool c "placeNameOnNewLine" defaultCte.placeNameOnNewLine
              indentName = getBool c "indentName" defaultCte.indentName
              placeColumnsOnNewLine = getBool c "placeColumnsOnNewLine" defaultCte.placeColumnsOnNewLine
              columnAlignment = getEnum c "columnAlignment" defaultCte.columnAlignment
              placeAsOnNewLine = getBool c "placeAsOnNewLine" defaultCte.placeAsOnNewLine
              asAlignment = getEnum c "asAlignment" defaultCte.asAlignment }
        | None -> defaultCte

    let variables =
        match getOpt root "variables" with
        | Some v ->
            { alignDataTypesAndValues = getBool v "alignDataTypesAndValues" defaultVariables.alignDataTypesAndValues
              addSpaceBetweenDataTypeAndPrecision =
                getBool v "addSpaceBetweenDataTypeAndPrecision" defaultVariables.addSpaceBetweenDataTypeAndPrecision
              placeAssignedValueOnNewLineIfLongerThanMaxLineLength =
                getBool
                    v
                    "placeAssignedValueOnNewLineIfLongerThanMaxLineLength"
                    defaultVariables.placeAssignedValueOnNewLineIfLongerThanMaxLineLength
              placeEqualsSignOnNewLine = getBool v "placeEqualsSignOnNewLine" defaultVariables.placeEqualsSignOnNewLine }
        | None -> defaultVariables

    let joinStatements =
        match getOpt root "joinStatements" with
        | Some js ->
            let join =
                match getOpt js "join" with
                | Some j ->
                    { placeOnNewLine = getBool j "placeOnNewLine" defaultJoin.placeOnNewLine
                      keywordAlignment = getEnum j "keywordAlignment" defaultJoin.keywordAlignment
                      insertEmptyLineBetweenJoinClauses =
                        getBool j "insertEmptyLineBetweenJoinClauses" defaultJoin.insertEmptyLineBetweenJoinClauses
                      placeJoinTableOnNewLine = getBool j "placeJoinTableOnNewLine" defaultJoin.placeJoinTableOnNewLine
                      indentJoinTable = getBool j "indentJoinTable" defaultJoin.indentJoinTable }
                | None -> defaultJoin

            let on =
                match getOpt js "on" with
                | Some o ->
                    { placeOnNewLine = getBool o "placeOnNewLine" defaultOn.placeOnNewLine
                      keywordAlignment = getEnum o "keywordAlignment" defaultOn.keywordAlignment
                      placeConditionOnNewLine = getBool o "placeConditionOnNewLine" defaultOn.placeConditionOnNewLine
                      conditionAlignment = getEnum o "conditionAlignment" defaultOn.conditionAlignment }
                | None -> defaultOn

            { join = join; on = on }
        | None -> defaultJoinStatements

    let insertStatements =
        match getOpt root "insertStatements" with
        | Some is ->
            let columns =
                match getOpt is "columns" with
                | Some c ->
                    { parenthesisStyle = getEnum c "parenthesisStyle" defaultInsertColumns.parenthesisStyle
                      indentContents = getBool c "indentContents" defaultInsertColumns.indentContents
                      placeSubsequentColumnsOnNewLines =
                        getEnum
                            c
                            "placeSubsequentColumnsOnNewLines"
                            defaultInsertColumns.placeSubsequentColumnsOnNewLines }
                | None -> defaultInsertColumns

            let values =
                match getOpt is "values" with
                | Some v ->
                    { parenthesisStyle = getEnum v "parenthesisStyle" defaultInsertValues.parenthesisStyle
                      indentContents = getBool v "indentContents" defaultInsertValues.indentContents
                      placeSubsequentValuesOnNewLines =
                        getEnum v "placeSubsequentValuesOnNewLines" defaultInsertValues.placeSubsequentValuesOnNewLines }
                | None -> defaultInsertValues

            { columns = columns; values = values }
        | None -> defaultInsertStatements

    let functionCalls =
        match getOpt root "functionCalls" with
        | Some fc ->
            { placeArgumentsOnNewLines =
                getEnum fc "placeArgumentsOnNewLines" defaultFunctionCalls.placeArgumentsOnNewLines
              addSpacesAroundParentheses =
                getBool fc "addSpacesAroundParentheses" defaultFunctionCalls.addSpacesAroundParentheses
              addSpacesAroundArgumentList =
                getBool fc "addSpacesAroundArgumentList" defaultFunctionCalls.addSpacesAroundArgumentList
              addSpaceBetweenEmptyParentheses =
                getBool fc "addSpaceBetweenEmptyParentheses" defaultFunctionCalls.addSpaceBetweenEmptyParentheses }
        | None -> defaultFunctionCalls

    let caseExpressions =
        match getOpt root "caseExpressions" with
        | Some ce ->
            { placeExpressionOnNewLine =
                getBool ce "placeExpressionOnNewLine" defaultCaseExpressions.placeExpressionOnNewLine
              placeFirstWhenOnNewLine =
                getEnum ce "placeFirstWhenOnNewLine" defaultCaseExpressions.placeFirstWhenOnNewLine
              whenAlignment = getEnum ce "whenAlignment" defaultCaseExpressions.whenAlignment
              placeThenOnNewLine = getBool ce "placeThenOnNewLine" defaultCaseExpressions.placeThenOnNewLine
              thenAlignment = getEnum ce "thenAlignment" defaultCaseExpressions.thenAlignment
              placeElseOnNewLine = getBool ce "placeElseOnNewLine" defaultCaseExpressions.placeElseOnNewLine
              alignElseToWhen = getBool ce "alignElseToWhen" defaultCaseExpressions.alignElseToWhen
              placeEndOnNewLine = getBool ce "placeEndOnNewLine" defaultCaseExpressions.placeEndOnNewLine
              endAlignment = getEnum ce "endAlignment" defaultCaseExpressions.endAlignment
              collapseShortCaseExpressions =
                getBool ce "collapseShortCaseExpressions" defaultCaseExpressions.collapseShortCaseExpressions
              collapseCaseExpressionsShorterThan =
                getInt ce "collapseCaseExpressionsShorterThan" defaultCaseExpressions.collapseCaseExpressionsShorterThan }
        | None -> defaultCaseExpressions

    let operators =
        match getOpt root "operators" with
        | Some ops ->
            let comparison =
                match getOpt ops "comparison" with
                | Some c ->
                    { align = getBool c "align" defaultComparison.align
                      addSpacesAround = getBool c "addSpacesAround" defaultComparison.addSpacesAround }
                | None -> defaultComparison

            let arithmetic =
                match getOpt ops "arithmetic" with
                | Some a -> { addSpacesAround = getBool a "addSpacesAround" defaultArithmetic.addSpacesAround }
                | None -> defaultArithmetic

            let andOr =
                match getOpt ops "andOr" with
                | Some ao ->
                    { placeOnNewLine = getEnum ao "placeOnNewLine" defaultAndOr.placeOnNewLine
                      alignment = getEnum ao "alignment" defaultAndOr.alignment
                      placeKeywordBeforeCondition =
                        getBool ao "placeKeywordBeforeCondition" defaultAndOr.placeKeywordBeforeCondition }
                | None -> defaultAndOr

            let between =
                match getOpt ops "between" with
                | Some b ->
                    { placeOnNewLine = getBool b "placeOnNewLine" defaultBetween.placeOnNewLine
                      placeAndKeywordOnNewLine =
                        getBool b "placeAndKeywordOnNewLine" defaultBetween.placeAndKeywordOnNewLine
                      andAlignment = getEnum b "andAlignment" defaultBetween.andAlignment }
                | None -> defaultBetween

            let inOp =
                match getOpt ops "in" with
                | Some i ->
                    { placeOpeningParenthesisOnNewLine =
                        getBool i "placeOpeningParenthesisOnNewLine" defaultIn.placeOpeningParenthesisOnNewLine
                      alignment = getEnum i "alignment" defaultIn.alignment
                      placeFirstValueOnNewLine = getEnum i "placeFirstValueOnNewLine" defaultIn.placeFirstValueOnNewLine
                      placeSubsequentValuesOnNewLines =
                        getEnum i "placeSubsequentValuesOnNewLines" defaultIn.placeSubsequentValuesOnNewLines
                      addSpaceAroundInContents = getBool i "addSpaceAroundInContents" defaultIn.addSpaceAroundInContents }
                | None -> defaultIn

            { comparison = comparison
              arithmetic = arithmetic
              andOr = andOr
              between = between
              ``in`` = inOp }
        | None -> defaultOperators

    let formatterExtensions =
        match getOpt root "formatterExtensions" with
        | Some ext ->
            let cte =
                match getOpt ext "cte" with
                | Some c ->
                    { omitLeadingSemicolon = getBool c "omitLeadingSemicolon" defaultCteExtensions.omitLeadingSemicolon }
                | None -> defaultCteExtensions

            let setOps =
                match getOpt ext "setOperations" with
                | Some so ->
                    { blankLinesAroundOperators =
                        getBool so "blankLinesAroundOperators" defaultSetOperationsExtensions.blankLinesAroundOperators }
                | None -> defaultSetOperationsExtensions

            { cte = cte; setOperations = setOps }
        | None -> defaultFormatterExtensions

    { whitespace = whitespace
      lists = lists
      parentheses = parentheses
      casing = casing
      dml = dml
      ddl = ddl
      controlFlow = controlFlow
      cte = cte
      variables = variables
      joinStatements = joinStatements
      insertStatements = insertStatements
      functionCalls = functionCalls
      caseExpressions = caseExpressions
      operators = operators
      formatterExtensions = formatterExtensions }
