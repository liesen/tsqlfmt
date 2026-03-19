/// Configuration model matching the formattingstyle-schema.json / SQL Prompt format.
module TSqlFormatter.Config

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

// ─── Enums ───

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type CasingStyle =
    | [<JsonPropertyName("leaveAsIs")>] LeaveAsIs = 0
    | [<JsonPropertyName("lowercase")>] Lowercase = 1
    | [<JsonPropertyName("uppercase")>] Uppercase = 2
    | [<JsonPropertyName("lowerCamelCase")>] LowerCamelCase = 3
    | [<JsonPropertyName("upperCamelCase")>] UpperCamelCase = 4

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

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type ClauseAlignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("toFirstListItem")>] ToFirstListItem = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type Alignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("beforeFirstListItem")>] BeforeFirstListItem = 2
    | [<JsonPropertyName("toFirstListItem")>] ToFirstListItem = 3
    | [<JsonPropertyName("indented")>] Indented = 4

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type JoinKeywordAlignment =
    | [<JsonPropertyName("toFrom")>] ToFrom = 0
    | [<JsonPropertyName("rightAlignedToFrom")>] RightAlignedToFrom = 1
    | [<JsonPropertyName("toTable")>] ToTable = 2
    | [<JsonPropertyName("indented")>] Indented = 3

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type OnKeywordAlignment =
    | [<JsonPropertyName("toJoin")>] ToJoin = 0
    | [<JsonPropertyName("rightAlignedToJoin")>] RightAlignedToJoin = 1
    | [<JsonPropertyName("rightAlignedToInner")>] RightAlignedToInner = 2
    | [<JsonPropertyName("toTable")>] ToTable = 3
    | [<JsonPropertyName("indented")>] Indented = 4

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type WhenAlignment =
    | [<JsonPropertyName("indentedFromCase")>] IndentedFromCase = 0
    | [<JsonPropertyName("toCase")>] ToCase = 1
    | [<JsonPropertyName("toFirstItem")>] ToFirstItem = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type EndAlignment =
    | [<JsonPropertyName("toCase")>] ToCase = 0
    | [<JsonPropertyName("toWhen")>] ToWhen = 1
    | [<JsonPropertyName("rightAlignedToWhen")>] RightAlignedToWhen = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type InAlignment =
    | [<JsonPropertyName("leftAligned")>] LeftAligned = 0
    | [<JsonPropertyName("rightAligned")>] RightAligned = 1
    | [<JsonPropertyName("indented")>] Indented = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type CommaAlignment =
    | [<JsonPropertyName("beforeItem")>] BeforeItem = 0
    | [<JsonPropertyName("toList")>] ToList = 1
    | [<JsonPropertyName("toStatement")>] ToStatement = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type SpacesOrTabs =
    | [<JsonPropertyName("spaces")>] Spaces = 0
    | [<JsonPropertyName("tabs")>] Tabs = 1
    | [<JsonPropertyName("tabsIfPossible")>] TabsIfPossible = 2

[<JsonConverter(typeof<JsonStringEnumConverter>)>]
type WhiteSpaceBeforeSemiColon =
    | [<JsonPropertyName("none")>] None = 0
    | [<JsonPropertyName("spaceBefore")>] SpaceBefore = 1
    | [<JsonPropertyName("newLineBefore")>] NewLineBefore = 2

// ─── Config Records ───

type NewLinesConfig = {
    [<JsonPropertyName("preserveExistingEmptyLinesBetweenStatements")>]
    preserveExistingEmptyLinesBetweenStatements: bool
    [<JsonPropertyName("preserveExistingEmptyLinesAfterBatchSeparator")>]
    preserveExistingEmptyLinesAfterBatchSeparator: bool
    [<JsonPropertyName("emptyLinesBetweenStatements")>]
    emptyLinesBetweenStatements: int
    [<JsonPropertyName("emptyLinesAfterBatchSeparator")>]
    emptyLinesAfterBatchSeparator: int
}

type WhitespaceConfig = {
    [<JsonPropertyName("spacesOrTabs")>]
    spacesOrTabs: SpacesOrTabs
    [<JsonPropertyName("numberOfSpacesInTabs")>]
    numberOfSpacesInTabs: int
    [<JsonPropertyName("wrapLongLines")>]
    wrapLongLines: bool
    [<JsonPropertyName("wrapLinesLongerThan")>]
    wrapLinesLongerThan: int
    [<JsonPropertyName("whiteSpaceBeforeSemiColon")>]
    whiteSpaceBeforeSemiColon: WhiteSpaceBeforeSemiColon
    [<JsonPropertyName("newLines")>]
    newLines: NewLinesConfig
}

type ListsConfig = {
    [<JsonPropertyName("placeFirstItemOnNewLine")>]
    placeFirstItemOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("placeSubsequentItemsOnNewLines")>]
    placeSubsequentItemsOnNewLines: PlaceOnNewLine
    [<JsonPropertyName("alignSubsequentItemsWithFirstItem")>]
    alignSubsequentItemsWithFirstItem: bool
    [<JsonPropertyName("alignItemsAcrossClauses")>]
    alignItemsAcrossClauses: bool
    [<JsonPropertyName("indentListItems")>]
    indentListItems: bool
    [<JsonPropertyName("alignItemsToTabStops")>]
    alignItemsToTabStops: bool
    [<JsonPropertyName("alignAliases")>]
    alignAliases: bool
    [<JsonPropertyName("alignComments")>]
    alignComments: bool
    [<JsonPropertyName("placeCommasBeforeItems")>]
    placeCommasBeforeItems: bool
    [<JsonPropertyName("addSpaceBeforeComma")>]
    addSpaceBeforeComma: bool
    [<JsonPropertyName("addSpaceAfterComma")>]
    addSpaceAfterComma: bool
    [<JsonPropertyName("commaAlignment")>]
    commaAlignment: CommaAlignment
}

type ParenthesesConfig = {
    [<JsonPropertyName("parenthesisStyle")>]
    parenthesisStyle: ParenthesisStyle
    [<JsonPropertyName("indentParenthesesContents")>]
    indentParenthesesContents: bool
    [<JsonPropertyName("collapseShortParenthesisContents")>]
    collapseShortParenthesisContents: bool
    [<JsonPropertyName("collapseParenthesesShorterThan")>]
    collapseParenthesesShorterThan: int
    [<JsonPropertyName("addSpacesAroundParentheses")>]
    addSpacesAroundParentheses: bool
    [<JsonPropertyName("addSpacesInsideParentheses")>]
    addSpacesInsideParentheses: bool
}

type CasingConfig = {
    [<JsonPropertyName("reservedKeywords")>]
    reservedKeywords: CasingStyle
    [<JsonPropertyName("builtInFunctions")>]
    builtInFunctions: CasingStyle
    [<JsonPropertyName("builtInDataTypes")>]
    builtInDataTypes: CasingStyle
    [<JsonPropertyName("globalVariables")>]
    globalVariables: CasingStyle
    [<JsonPropertyName("useObjectDefinitionCase")>]
    useObjectDefinitionCase: bool
}

type DmlClausesConfig = {
    [<JsonPropertyName("clauseAlignment")>]
    clauseAlignment: ClauseAlignment
    [<JsonPropertyName("clauseIndentation")>]
    clauseIndentation: int
}

type DmlListItemsConfig = {
    [<JsonPropertyName("placeFromTableOnNewLine")>]
    placeFromTableOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("placeWhereConditionOnNewLine")>]
    placeWhereConditionOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("placeGroupByAndOrderByOnNewLine")>]
    placeGroupByAndOrderByOnNewLine: PlaceOnNewLine
}

type DmlConfig = {
    [<JsonPropertyName("placeInsertTableOnNewLine")>]
    placeInsertTableOnNewLine: bool
    [<JsonPropertyName("placeDistinctAndTopClausesOnNewLine")>]
    placeDistinctAndTopClausesOnNewLine: bool
    [<JsonPropertyName("addNewLineAfterDistinctAndTopClauses")>]
    addNewLineAfterDistinctAndTopClauses: bool
    [<JsonPropertyName("collapseShortStatements")>]
    collapseShortStatements: bool
    [<JsonPropertyName("collapseStatementsShorterThan")>]
    collapseStatementsShorterThan: int
    [<JsonPropertyName("collapseShortSubqueries")>]
    collapseShortSubqueries: bool
    [<JsonPropertyName("collapseSubqueriesShorterThan")>]
    collapseSubqueriesShorterThan: int
    [<JsonPropertyName("clauses")>]
    clauses: DmlClausesConfig
    [<JsonPropertyName("listItems")>]
    listItems: DmlListItemsConfig
}

type DdlConfig = {
    [<JsonPropertyName("parenthesisStyle")>]
    parenthesisStyle: ParenthesisStyle
    [<JsonPropertyName("indentParenthesesContents")>]
    indentParenthesesContents: bool
    [<JsonPropertyName("alignDataTypesAndConstraints")>]
    alignDataTypesAndConstraints: bool
    [<JsonPropertyName("placeConstraintsOnNewLines")>]
    placeConstraintsOnNewLines: bool
    [<JsonPropertyName("placeConstraintColumnsOnNewLines")>]
    placeConstraintColumnsOnNewLines: PlaceOnNewLine
    [<JsonPropertyName("indentClauses")>]
    indentClauses: bool
    [<JsonPropertyName("placeFirstProcedureParameterOnNewLine")>]
    placeFirstProcedureParameterOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("collapseShortStatements")>]
    collapseShortStatements: bool
    [<JsonPropertyName("collapseStatementsShorterThan")>]
    collapseStatementsShorterThan: int
}

type ControlFlowConfig = {
    [<JsonPropertyName("placeBeginAndEndOnNewLine")>]
    placeBeginAndEndOnNewLine: bool
    [<JsonPropertyName("indentBeginAndEndKeywords")>]
    indentBeginAndEndKeywords: bool
    [<JsonPropertyName("indentContentsOfStatements")>]
    indentContentsOfStatements: bool
    [<JsonPropertyName("collapseShortStatements")>]
    collapseShortStatements: bool
    [<JsonPropertyName("collapseStatementsShorterThan")>]
    collapseStatementsShorterThan: int
}

type CteConfig = {
    [<JsonPropertyName("parenthesisStyle")>]
    parenthesisStyle: ParenthesisStyle
    [<JsonPropertyName("indentContents")>]
    indentContents: bool
    [<JsonPropertyName("placeNameOnNewLine")>]
    placeNameOnNewLine: bool
    [<JsonPropertyName("indentName")>]
    indentName: bool
    [<JsonPropertyName("placeColumnsOnNewLine")>]
    placeColumnsOnNewLine: bool
    [<JsonPropertyName("columnAlignment")>]
    columnAlignment: Alignment
    [<JsonPropertyName("placeAsOnNewLine")>]
    placeAsOnNewLine: bool
    [<JsonPropertyName("asAlignment")>]
    asAlignment: Alignment
}

type VariablesConfig = {
    [<JsonPropertyName("alignDataTypesAndValues")>]
    alignDataTypesAndValues: bool
    [<JsonPropertyName("addSpaceBetweenDataTypeAndPrecision")>]
    addSpaceBetweenDataTypeAndPrecision: bool
    [<JsonPropertyName("placeAssignedValueOnNewLineIfLongerThanMaxLineLength")>]
    placeAssignedValueOnNewLineIfLongerThanMaxLineLength: bool
    [<JsonPropertyName("placeEqualsSignOnNewLine")>]
    placeEqualsSignOnNewLine: bool
}

type JoinConfig = {
    [<JsonPropertyName("placeOnNewLine")>]
    placeOnNewLine: bool
    [<JsonPropertyName("keywordAlignment")>]
    keywordAlignment: JoinKeywordAlignment
    [<JsonPropertyName("insertEmptyLineBetweenJoinClauses")>]
    insertEmptyLineBetweenJoinClauses: bool
    [<JsonPropertyName("placeJoinTableOnNewLine")>]
    placeJoinTableOnNewLine: bool
    [<JsonPropertyName("indentJoinTable")>]
    indentJoinTable: bool
}

type OnConfig = {
    [<JsonPropertyName("placeOnNewLine")>]
    placeOnNewLine: bool
    [<JsonPropertyName("keywordAlignment")>]
    keywordAlignment: OnKeywordAlignment
    [<JsonPropertyName("placeConditionOnNewLine")>]
    placeConditionOnNewLine: bool
    [<JsonPropertyName("conditionAlignment")>]
    conditionAlignment: Alignment
}

type JoinStatementsConfig = {
    [<JsonPropertyName("join")>]
    join: JoinConfig
    [<JsonPropertyName("on")>]
    on: OnConfig
}

type InsertColumnsConfig = {
    [<JsonPropertyName("parenthesisStyle")>]
    parenthesisStyle: ParenthesisStyle
    [<JsonPropertyName("indentContents")>]
    indentContents: bool
    [<JsonPropertyName("placeSubsequentColumnsOnNewLines")>]
    placeSubsequentColumnsOnNewLines: PlaceOnNewLine
}

type InsertValuesConfig = {
    [<JsonPropertyName("parenthesisStyle")>]
    parenthesisStyle: ParenthesisStyle
    [<JsonPropertyName("indentContents")>]
    indentContents: bool
    [<JsonPropertyName("placeSubsequentValuesOnNewLines")>]
    placeSubsequentValuesOnNewLines: PlaceOnNewLine
}

type InsertStatementsConfig = {
    [<JsonPropertyName("columns")>]
    columns: InsertColumnsConfig
    [<JsonPropertyName("values")>]
    values: InsertValuesConfig
}

type FunctionCallsConfig = {
    [<JsonPropertyName("placeArgumentsOnNewLines")>]
    placeArgumentsOnNewLines: PlaceOnNewLine
    [<JsonPropertyName("addSpacesAroundParentheses")>]
    addSpacesAroundParentheses: bool
    [<JsonPropertyName("addSpacesAroundArgumentList")>]
    addSpacesAroundArgumentList: bool
    [<JsonPropertyName("addSpaceBetweenEmptyParentheses")>]
    addSpaceBetweenEmptyParentheses: bool
}

type CaseExpressionsConfig = {
    [<JsonPropertyName("placeExpressionOnNewLine")>]
    placeExpressionOnNewLine: bool
    [<JsonPropertyName("placeFirstWhenOnNewLine")>]
    placeFirstWhenOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("whenAlignment")>]
    whenAlignment: WhenAlignment
    [<JsonPropertyName("placeThenOnNewLine")>]
    placeThenOnNewLine: bool
    [<JsonPropertyName("thenAlignment")>]
    thenAlignment: Alignment
    [<JsonPropertyName("placeElseOnNewLine")>]
    placeElseOnNewLine: bool
    [<JsonPropertyName("alignElseToWhen")>]
    alignElseToWhen: bool
    [<JsonPropertyName("placeEndOnNewLine")>]
    placeEndOnNewLine: bool
    [<JsonPropertyName("endAlignment")>]
    endAlignment: EndAlignment
    [<JsonPropertyName("collapseShortCaseExpressions")>]
    collapseShortCaseExpressions: bool
    [<JsonPropertyName("collapseCaseExpressionsShorterThan")>]
    collapseCaseExpressionsShorterThan: int
}

type ComparisonConfig = {
    [<JsonPropertyName("align")>]
    align: bool
    [<JsonPropertyName("addSpacesAround")>]
    addSpacesAround: bool
}

type ArithmeticConfig = {
    [<JsonPropertyName("addSpacesAround")>]
    addSpacesAround: bool
}

type AndOrConfig = {
    [<JsonPropertyName("placeOnNewLine")>]
    placeOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("alignment")>]
    alignment: Alignment
    [<JsonPropertyName("placeKeywordBeforeCondition")>]
    placeKeywordBeforeCondition: bool
}

type BetweenConfig = {
    [<JsonPropertyName("placeOnNewLine")>]
    placeOnNewLine: bool
    [<JsonPropertyName("placeAndKeywordOnNewLine")>]
    placeAndKeywordOnNewLine: bool
    [<JsonPropertyName("andAlignment")>]
    andAlignment: Alignment
}

type InConfig = {
    [<JsonPropertyName("placeOpeningParenthesisOnNewLine")>]
    placeOpeningParenthesisOnNewLine: bool
    [<JsonPropertyName("alignment")>]
    alignment: InAlignment
    [<JsonPropertyName("placeFirstValueOnNewLine")>]
    placeFirstValueOnNewLine: PlaceOnNewLine
    [<JsonPropertyName("placeSubsequentValuesOnNewLines")>]
    placeSubsequentValuesOnNewLines: PlaceOnNewLine
    [<JsonPropertyName("addSpaceAroundInContents")>]
    addSpaceAroundInContents: bool
}

type OperatorsConfig = {
    [<JsonPropertyName("comparison")>]
    comparison: ComparisonConfig
    [<JsonPropertyName("arithmetic")>]
    arithmetic: ArithmeticConfig
    [<JsonPropertyName("andOr")>]
    andOr: AndOrConfig
    [<JsonPropertyName("between")>]
    between: BetweenConfig
    [<JsonPropertyName("in")>]
    ``in``: InConfig
}

/// Custom formatter-specific options that are not part of SQL Prompt schema.
type SetOperationsExtensionsConfig = {
    [<JsonPropertyName("blankLinesAroundOperators")>]
    blankLinesAroundOperators: bool
}

/// Namespace for custom formatter-specific options.
type FormatterExtensionsConfig = {
    [<JsonPropertyName("setOperations")>]
    setOperations: SetOperationsExtensionsConfig
}

type FormattingStyle = {
    [<JsonPropertyName("whitespace")>]
    whitespace: WhitespaceConfig
    [<JsonPropertyName("lists")>]
    lists: ListsConfig
    [<JsonPropertyName("parentheses")>]
    parentheses: ParenthesesConfig
    [<JsonPropertyName("casing")>]
    casing: CasingConfig
    [<JsonPropertyName("dml")>]
    dml: DmlConfig
    [<JsonPropertyName("ddl")>]
    ddl: DdlConfig
    [<JsonPropertyName("controlFlow")>]
    controlFlow: ControlFlowConfig
    [<JsonPropertyName("cte")>]
    cte: CteConfig
    [<JsonPropertyName("variables")>]
    variables: VariablesConfig
    [<JsonPropertyName("joinStatements")>]
    joinStatements: JoinStatementsConfig
    [<JsonPropertyName("insertStatements")>]
    insertStatements: InsertStatementsConfig
    [<JsonPropertyName("functionCalls")>]
    functionCalls: FunctionCallsConfig
    [<JsonPropertyName("caseExpressions")>]
    caseExpressions: CaseExpressionsConfig
    [<JsonPropertyName("operators")>]
    operators: OperatorsConfig
    [<JsonPropertyName("formatterExtensions")>]
    formatterExtensions: FormatterExtensionsConfig
}

// ─── Defaults ───

let defaultNewLines = {
    preserveExistingEmptyLinesBetweenStatements = true
    preserveExistingEmptyLinesAfterBatchSeparator = true
    emptyLinesBetweenStatements = 1
    emptyLinesAfterBatchSeparator = 1
}

let defaultWhitespace = {
    spacesOrTabs = SpacesOrTabs.Spaces
    numberOfSpacesInTabs = 4
    wrapLongLines = true
    wrapLinesLongerThan = 120
    whiteSpaceBeforeSemiColon = WhiteSpaceBeforeSemiColon.None
    newLines = defaultNewLines
}

let defaultLists = {
    placeFirstItemOnNewLine = PlaceOnNewLine.Never
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
    commaAlignment = CommaAlignment.ToList
}

let defaultParentheses = {
    parenthesisStyle = ParenthesisStyle.CompactSimple
    indentParenthesesContents = false
    collapseShortParenthesisContents = false
    collapseParenthesesShorterThan = 80
    addSpacesAroundParentheses = true
    addSpacesInsideParentheses = false
}

let defaultCasing = {
    reservedKeywords = CasingStyle.LeaveAsIs
    builtInFunctions = CasingStyle.LeaveAsIs
    builtInDataTypes = CasingStyle.LeaveAsIs
    globalVariables = CasingStyle.LeaveAsIs
    useObjectDefinitionCase = false
}

let defaultDmlClauses = {
    clauseAlignment = ClauseAlignment.LeftAligned
    clauseIndentation = 0
}

let defaultDmlListItems = {
    placeFromTableOnNewLine = PlaceOnNewLine.Never
    placeWhereConditionOnNewLine = PlaceOnNewLine.Never
    placeGroupByAndOrderByOnNewLine = PlaceOnNewLine.Never
}

let defaultDml = {
    placeInsertTableOnNewLine = false
    placeDistinctAndTopClausesOnNewLine = false
    addNewLineAfterDistinctAndTopClauses = false
    collapseShortStatements = false
    collapseStatementsShorterThan = 80
    collapseShortSubqueries = false
    collapseSubqueriesShorterThan = 80
    clauses = defaultDmlClauses
    listItems = defaultDmlListItems
}

let defaultDdl = {
    parenthesisStyle = ParenthesisStyle.CompactSimple
    indentParenthesesContents = false
    alignDataTypesAndConstraints = true
    placeConstraintsOnNewLines = false
    placeConstraintColumnsOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
    indentClauses = false
    placeFirstProcedureParameterOnNewLine = PlaceOnNewLine.IfMultipleItems
    collapseShortStatements = false
    collapseStatementsShorterThan = 80
}

let defaultControlFlow = {
    placeBeginAndEndOnNewLine = true
    indentBeginAndEndKeywords = false
    indentContentsOfStatements = true
    collapseShortStatements = false
    collapseStatementsShorterThan = 80
}

let defaultCte = {
    parenthesisStyle = ParenthesisStyle.CompactSimple
    indentContents = false
    placeNameOnNewLine = false
    indentName = false
    placeColumnsOnNewLine = false
    columnAlignment = Alignment.LeftAligned
    placeAsOnNewLine = true
    asAlignment = Alignment.LeftAligned
}

let defaultVariables = {
    alignDataTypesAndValues = true
    addSpaceBetweenDataTypeAndPrecision = false
    placeAssignedValueOnNewLineIfLongerThanMaxLineLength = true
    placeEqualsSignOnNewLine = false
}

let defaultJoin = {
    placeOnNewLine = true
    keywordAlignment = JoinKeywordAlignment.ToFrom
    insertEmptyLineBetweenJoinClauses = false
    placeJoinTableOnNewLine = false
    indentJoinTable = true
}

let defaultOn = {
    placeOnNewLine = true
    keywordAlignment = OnKeywordAlignment.ToJoin
    placeConditionOnNewLine = false
    conditionAlignment = Alignment.LeftAligned
}

let defaultJoinStatements = {
    join = defaultJoin
    on = defaultOn
}

let defaultInsertColumns = {
    parenthesisStyle = ParenthesisStyle.ExpandedToStatement
    indentContents = true
    placeSubsequentColumnsOnNewLines = PlaceOnNewLine.Always
}

let defaultInsertValues = {
    parenthesisStyle = ParenthesisStyle.CompactToStatement
    indentContents = false
    placeSubsequentValuesOnNewLines = PlaceOnNewLine.Never
}

let defaultInsertStatements = {
    columns = defaultInsertColumns
    values = defaultInsertValues
}

let defaultFunctionCalls = {
    placeArgumentsOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
    addSpacesAroundParentheses = false
    addSpacesAroundArgumentList = false
    addSpaceBetweenEmptyParentheses = false
}

let defaultCaseExpressions = {
    placeExpressionOnNewLine = true
    placeFirstWhenOnNewLine = PlaceOnNewLine.Always
    whenAlignment = WhenAlignment.IndentedFromCase
    placeThenOnNewLine = false
    thenAlignment = Alignment.Indented
    placeElseOnNewLine = true
    alignElseToWhen = true
    placeEndOnNewLine = true
    endAlignment = EndAlignment.ToCase
    collapseShortCaseExpressions = false
    collapseCaseExpressionsShorterThan = 80
}

let defaultComparison = {
    align = false
    addSpacesAround = true
}

let defaultArithmetic = {
    addSpacesAround = true
}

let defaultAndOr = {
    placeOnNewLine = PlaceOnNewLine.Always
    alignment = Alignment.LeftAligned
    placeKeywordBeforeCondition = true
}

let defaultBetween = {
    placeOnNewLine = true
    placeAndKeywordOnNewLine = false
    andAlignment = Alignment.LeftAligned
}

let defaultIn = {
    placeOpeningParenthesisOnNewLine = false
    alignment = InAlignment.LeftAligned
    placeFirstValueOnNewLine = PlaceOnNewLine.IfLongerThanMaxLineLength
    placeSubsequentValuesOnNewLines = PlaceOnNewLine.IfLongerThanMaxLineLength
    addSpaceAroundInContents = false
}

let defaultOperators = {
    comparison = defaultComparison
    arithmetic = defaultArithmetic
    andOr = defaultAndOr
    between = defaultBetween
    ``in`` = defaultIn
}

let defaultSetOperationsExtensions = {
    blankLinesAroundOperators = true
}

let defaultFormatterExtensions = {
    setOperations = defaultSetOperationsExtensions
}

let defaultStyle : FormattingStyle = {
    whitespace = defaultWhitespace
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
    formatterExtensions = defaultFormatterExtensions
}

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

let private getJsonEnum<'T when 'T :> System.Enum and 'T : struct> (el: JsonElement) (name: string) (def: 'T) : 'T =
    match getJsonOpt el name with
    | Some v ->
        let s = v.GetString()
        let opts = JsonSerializerOptions()
        opts.Converters.Add(JsonStringEnumConverter())
        JsonSerializer.Deserialize<'T>("\"" + s + "\"", opts)
    | None -> def

/// Merge a partially-specified JSON config over the defaults.
let loadConfig (path: string) : FormattingStyle =
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
                    { preserveExistingEmptyLinesBetweenStatements = getBool n "preserveExistingEmptyLinesBetweenStatements" defaultNewLines.preserveExistingEmptyLinesBetweenStatements
                      preserveExistingEmptyLinesAfterBatchSeparator = getBool n "preserveExistingEmptyLinesAfterBatchSeparator" defaultNewLines.preserveExistingEmptyLinesAfterBatchSeparator
                      emptyLinesBetweenStatements = getInt n "emptyLinesBetweenStatements" defaultNewLines.emptyLinesBetweenStatements
                      emptyLinesAfterBatchSeparator = getInt n "emptyLinesAfterBatchSeparator" defaultNewLines.emptyLinesAfterBatchSeparator }
                | None -> defaultNewLines
            { spacesOrTabs = getEnum ws "spacesOrTabs" defaultWhitespace.spacesOrTabs
              numberOfSpacesInTabs = getInt ws "numberOfSpacesInTabs" defaultWhitespace.numberOfSpacesInTabs
              wrapLongLines = getBool ws "wrapLongLines" defaultWhitespace.wrapLongLines
              wrapLinesLongerThan = getInt ws "wrapLinesLongerThan" defaultWhitespace.wrapLinesLongerThan
              whiteSpaceBeforeSemiColon = getEnum ws "whiteSpaceBeforeSemiColon" defaultWhitespace.whiteSpaceBeforeSemiColon
              newLines = nl }
        | None -> defaultWhitespace

    let lists =
        match getOpt root "lists" with
        | Some ls ->
            { placeFirstItemOnNewLine = getEnum ls "placeFirstItemOnNewLine" defaultLists.placeFirstItemOnNewLine
              placeSubsequentItemsOnNewLines = getEnum ls "placeSubsequentItemsOnNewLines" defaultLists.placeSubsequentItemsOnNewLines
              alignSubsequentItemsWithFirstItem = getBool ls "alignSubsequentItemsWithFirstItem" defaultLists.alignSubsequentItemsWithFirstItem
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
              indentParenthesesContents = getBool p "indentParenthesesContents" defaultParentheses.indentParenthesesContents
              collapseShortParenthesisContents = getBool p "collapseShortParenthesisContents" defaultParentheses.collapseShortParenthesisContents
              collapseParenthesesShorterThan = getInt p "collapseParenthesesShorterThan" defaultParentheses.collapseParenthesesShorterThan
              addSpacesAroundParentheses = getBool p "addSpacesAroundParentheses" defaultParentheses.addSpacesAroundParentheses
              addSpacesInsideParentheses = getBool p "addSpacesInsideParentheses" defaultParentheses.addSpacesInsideParentheses }
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
                    { placeFromTableOnNewLine = getEnum li "placeFromTableOnNewLine" defaultDmlListItems.placeFromTableOnNewLine
                      placeWhereConditionOnNewLine = getEnum li "placeWhereConditionOnNewLine" defaultDmlListItems.placeWhereConditionOnNewLine
                      placeGroupByAndOrderByOnNewLine = getEnum li "placeGroupByAndOrderByOnNewLine" defaultDmlListItems.placeGroupByAndOrderByOnNewLine }
                | None -> defaultDmlListItems
            { placeInsertTableOnNewLine = getBool d "placeInsertTableOnNewLine" defaultDml.placeInsertTableOnNewLine
              placeDistinctAndTopClausesOnNewLine = getBool d "placeDistinctAndTopClausesOnNewLine" defaultDml.placeDistinctAndTopClausesOnNewLine
              addNewLineAfterDistinctAndTopClauses = getBool d "addNewLineAfterDistinctAndTopClauses" defaultDml.addNewLineAfterDistinctAndTopClauses
              collapseShortStatements = getBool d "collapseShortStatements" defaultDml.collapseShortStatements
              collapseStatementsShorterThan = getInt d "collapseStatementsShorterThan" defaultDml.collapseStatementsShorterThan
              collapseShortSubqueries = getBool d "collapseShortSubqueries" defaultDml.collapseShortSubqueries
              collapseSubqueriesShorterThan = getInt d "collapseSubqueriesShorterThan" defaultDml.collapseSubqueriesShorterThan
              clauses = clauses
              listItems = listItems }
        | None -> defaultDml

    let ddl =
        match getOpt root "ddl" with
        | Some d ->
            { parenthesisStyle = getEnum d "parenthesisStyle" defaultDdl.parenthesisStyle
              indentParenthesesContents = getBool d "indentParenthesesContents" defaultDdl.indentParenthesesContents
              alignDataTypesAndConstraints = getBool d "alignDataTypesAndConstraints" defaultDdl.alignDataTypesAndConstraints
              placeConstraintsOnNewLines = getBool d "placeConstraintsOnNewLines" defaultDdl.placeConstraintsOnNewLines
              placeConstraintColumnsOnNewLines = getEnum d "placeConstraintColumnsOnNewLines" defaultDdl.placeConstraintColumnsOnNewLines
              indentClauses = getBool d "indentClauses" defaultDdl.indentClauses
              placeFirstProcedureParameterOnNewLine = getEnum d "placeFirstProcedureParameterOnNewLine" defaultDdl.placeFirstProcedureParameterOnNewLine
              collapseShortStatements = getBool d "collapseShortStatements" defaultDdl.collapseShortStatements
              collapseStatementsShorterThan = getInt d "collapseStatementsShorterThan" defaultDdl.collapseStatementsShorterThan }
        | None -> defaultDdl

    let controlFlow =
        match getOpt root "controlFlow" with
        | Some cf ->
            { placeBeginAndEndOnNewLine = getBool cf "placeBeginAndEndOnNewLine" defaultControlFlow.placeBeginAndEndOnNewLine
              indentBeginAndEndKeywords = getBool cf "indentBeginAndEndKeywords" defaultControlFlow.indentBeginAndEndKeywords
              indentContentsOfStatements = getBool cf "indentContentsOfStatements" defaultControlFlow.indentContentsOfStatements
              collapseShortStatements = getBool cf "collapseShortStatements" defaultControlFlow.collapseShortStatements
              collapseStatementsShorterThan = getInt cf "collapseStatementsShorterThan" defaultControlFlow.collapseStatementsShorterThan }
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
              addSpaceBetweenDataTypeAndPrecision = getBool v "addSpaceBetweenDataTypeAndPrecision" defaultVariables.addSpaceBetweenDataTypeAndPrecision
              placeAssignedValueOnNewLineIfLongerThanMaxLineLength = getBool v "placeAssignedValueOnNewLineIfLongerThanMaxLineLength" defaultVariables.placeAssignedValueOnNewLineIfLongerThanMaxLineLength
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
                      insertEmptyLineBetweenJoinClauses = getBool j "insertEmptyLineBetweenJoinClauses" defaultJoin.insertEmptyLineBetweenJoinClauses
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
                      placeSubsequentColumnsOnNewLines = getEnum c "placeSubsequentColumnsOnNewLines" defaultInsertColumns.placeSubsequentColumnsOnNewLines }
                | None -> defaultInsertColumns
            let values =
                match getOpt is "values" with
                | Some v ->
                    { parenthesisStyle = getEnum v "parenthesisStyle" defaultInsertValues.parenthesisStyle
                      indentContents = getBool v "indentContents" defaultInsertValues.indentContents
                      placeSubsequentValuesOnNewLines = getEnum v "placeSubsequentValuesOnNewLines" defaultInsertValues.placeSubsequentValuesOnNewLines }
                | None -> defaultInsertValues
            { columns = columns; values = values }
        | None -> defaultInsertStatements

    let functionCalls =
        match getOpt root "functionCalls" with
        | Some fc ->
            { placeArgumentsOnNewLines = getEnum fc "placeArgumentsOnNewLines" defaultFunctionCalls.placeArgumentsOnNewLines
              addSpacesAroundParentheses = getBool fc "addSpacesAroundParentheses" defaultFunctionCalls.addSpacesAroundParentheses
              addSpacesAroundArgumentList = getBool fc "addSpacesAroundArgumentList" defaultFunctionCalls.addSpacesAroundArgumentList
              addSpaceBetweenEmptyParentheses = getBool fc "addSpaceBetweenEmptyParentheses" defaultFunctionCalls.addSpaceBetweenEmptyParentheses }
        | None -> defaultFunctionCalls

    let caseExpressions =
        match getOpt root "caseExpressions" with
        | Some ce ->
            { placeExpressionOnNewLine = getBool ce "placeExpressionOnNewLine" defaultCaseExpressions.placeExpressionOnNewLine
              placeFirstWhenOnNewLine = getEnum ce "placeFirstWhenOnNewLine" defaultCaseExpressions.placeFirstWhenOnNewLine
              whenAlignment = getEnum ce "whenAlignment" defaultCaseExpressions.whenAlignment
              placeThenOnNewLine = getBool ce "placeThenOnNewLine" defaultCaseExpressions.placeThenOnNewLine
              thenAlignment = getEnum ce "thenAlignment" defaultCaseExpressions.thenAlignment
              placeElseOnNewLine = getBool ce "placeElseOnNewLine" defaultCaseExpressions.placeElseOnNewLine
              alignElseToWhen = getBool ce "alignElseToWhen" defaultCaseExpressions.alignElseToWhen
              placeEndOnNewLine = getBool ce "placeEndOnNewLine" defaultCaseExpressions.placeEndOnNewLine
              endAlignment = getEnum ce "endAlignment" defaultCaseExpressions.endAlignment
              collapseShortCaseExpressions = getBool ce "collapseShortCaseExpressions" defaultCaseExpressions.collapseShortCaseExpressions
              collapseCaseExpressionsShorterThan = getInt ce "collapseCaseExpressionsShorterThan" defaultCaseExpressions.collapseCaseExpressionsShorterThan }
        | None -> defaultCaseExpressions

    let operators =
        match getOpt root "operators" with
        | Some ops ->
            let comparison =
                match getOpt ops "comparison" with
                | Some c -> { align = getBool c "align" defaultComparison.align; addSpacesAround = getBool c "addSpacesAround" defaultComparison.addSpacesAround }
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
                      placeKeywordBeforeCondition = getBool ao "placeKeywordBeforeCondition" defaultAndOr.placeKeywordBeforeCondition }
                | None -> defaultAndOr
            let between =
                match getOpt ops "between" with
                | Some b ->
                    { placeOnNewLine = getBool b "placeOnNewLine" defaultBetween.placeOnNewLine
                      placeAndKeywordOnNewLine = getBool b "placeAndKeywordOnNewLine" defaultBetween.placeAndKeywordOnNewLine
                      andAlignment = getEnum b "andAlignment" defaultBetween.andAlignment }
                | None -> defaultBetween
            let inOp =
                match getOpt ops "in" with
                | Some i ->
                    { placeOpeningParenthesisOnNewLine = getBool i "placeOpeningParenthesisOnNewLine" defaultIn.placeOpeningParenthesisOnNewLine
                      alignment = getEnum i "alignment" defaultIn.alignment
                      placeFirstValueOnNewLine = getEnum i "placeFirstValueOnNewLine" defaultIn.placeFirstValueOnNewLine
                      placeSubsequentValuesOnNewLines = getEnum i "placeSubsequentValuesOnNewLines" defaultIn.placeSubsequentValuesOnNewLines
                      addSpaceAroundInContents = getBool i "addSpaceAroundInContents" defaultIn.addSpaceAroundInContents }
                | None -> defaultIn
            { comparison = comparison; arithmetic = arithmetic; andOr = andOr; between = between; ``in`` = inOp }
        | None -> defaultOperators

    let formatterExtensions =
        match getOpt root "formatterExtensions" with
        | Some ext ->
            let setOps =
                match getOpt ext "setOperations" with
                | Some so ->
                    { blankLinesAroundOperators = getBool so "blankLinesAroundOperators" defaultSetOperationsExtensions.blankLinesAroundOperators }
                | None -> defaultSetOperationsExtensions
            { setOperations = setOps }
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
