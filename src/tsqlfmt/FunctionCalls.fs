module TSqlFormatter.FunctionCalls

open TSqlFormatter.Doc
open TSqlFormatter.Style

type FunctionCallCombinator = Doc -> Doc list -> Doc

let private indentWidth (cfg: Style) = cfg.whitespace.numberOfSpacesInTabs

let private combineNameAndArgs (cfg: Style) (functionNameDoc: Doc) (argsDoc: Doc) : Doc =
    if cfg.functionCalls.addSpacesAroundParentheses then
        functionNameDoc <++> argsDoc
    else
        functionNameDoc <+> argsDoc

let private emptyArgsDoc (cfg: Style) : Doc =
    if cfg.functionCalls.addSpaceBetweenEmptyParentheses then
        text "( )"
    else
        text "()"

let private flatArgsDoc (cfg: Style) (argDocs: Doc list) : Doc =
    match argDocs with
    | [] -> emptyArgsDoc cfg
    | _ ->
        let separator = text ", "

        let openParen, closeParen =
            if cfg.functionCalls.addSpacesAroundArgumentList then
                text "( ", text " )"
            else
                text "(", text ")"

        openParen <+> join separator argDocs <+> closeParen

let private expandedArgsDoc (cfg: Style) (argDocs: Doc list) : Doc =
    match argDocs with
    | [] -> emptyArgsDoc cfg
    | _ ->
        let openingBreak =
            if cfg.functionCalls.addSpacesAroundArgumentList then
                line
            else
                softline

        let closingBreak =
            if cfg.functionCalls.addSpacesAroundArgumentList then
                line
            else
                softline

        text "("
        <+> nest (indentWidth cfg) (openingBreak <+> join (text "," <+> line) argDocs)
        <+> closingBreak
        <+> text ")"

let private chooseArgsDoc (cfg: Style) (flatDoc: Doc) (expandedDoc: Doc) (hasArgs: bool) : Doc =
    if not hasArgs then
        flatDoc
    else
        match cfg.functionCalls.placeArgumentsOnNewLines with
        | PlaceOnNewLine.Always -> expandedDoc
        | PlaceOnNewLine.Never -> flatDoc
        | _ -> group expandedDoc

let callDoc (cfg: Style) : FunctionCallCombinator =
    fun functionNameDoc argDocs ->
        let flatDoc = flatArgsDoc cfg argDocs
        let expandedDoc = expandedArgsDoc cfg argDocs
        let argsDoc = chooseArgsDoc cfg flatDoc expandedDoc (not (List.isEmpty argDocs))
        combineNameAndArgs cfg functionNameDoc argsDoc

let contentsDoc (cfg: Style) (functionNameDoc: Doc) (contentsDoc: Doc) : Doc =
    let flatDoc =
        if cfg.functionCalls.addSpacesAroundArgumentList then
            text "( " <+> contentsDoc <+> text " )"
        else
            text "(" <+> contentsDoc <+> text ")"

    let openingBreak =
        if cfg.functionCalls.addSpacesAroundArgumentList then
            line
        else
            softline

    let closingBreak =
        if cfg.functionCalls.addSpacesAroundArgumentList then
            line
        else
            softline

    let expandedDoc =
        text "("
        <+> nest (indentWidth cfg) (openingBreak <+> contentsDoc)
        <+> closingBreak
        <+> text ")"

    let argsDoc = chooseArgsDoc cfg flatDoc expandedDoc true
    combineNameAndArgs cfg functionNameDoc argsDoc
