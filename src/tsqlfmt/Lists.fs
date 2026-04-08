module TSqlFormatter.Lists

open TSqlFormatter.Doc
open TSqlFormatter.Style

/// List formatting is split into three layers:
///
/// - layout: where items go
/// - decoration: how items are punctuated
/// - anchoring: what the list attaches to
///
/// Examples:
/// - layout: `a, b, c` vs `a,` / `    b,` / `    c`
/// - decoration: `a, b, c` vs `a , b , c` vs `a` / `, b` / `, c`
/// - anchoring: `ORDER BY a, b` or `PARTITION BY a, b`
///
/// The goal is to keep these concerns composable so callers can reuse the same
/// comma decoration with a different layout, as in `OVER (PARTITION BY a, b)`.
type SequenceLayout =
    { placeFirstItemOnNewLine: bool
      firstItemIndent: int option
      subsequentItemsIndent: int option }

type CommaPlacement =
    | BeforeItems
    | AfterItems

type SequenceDecoration =
    { commaPlacement: CommaPlacement
      addSpaceBeforeComma: bool }

let sequenceDoc (layout: SequenceLayout) (items: Doc list) : Doc =
    let nestIfIndented indent doc =
        match indent with
        | Some spaces -> nest spaces doc
        | None -> doc

    let subsequentIndent = defaultArg layout.subsequentItemsIndent 0

    match items with
    | [] -> empty
    | [ singleItem ] -> nestIfIndented layout.firstItemIndent singleItem
    | firstItem :: remainingItems ->
        let firstItemDoc = nestIfIndented layout.firstItemIndent firstItem
        let remainingItemsDoc = join line remainingItems
        firstItemDoc <+> nest subsequentIndent (line <+> remainingItemsDoc)

let anchoredSequenceDoc (layout: SequenceLayout) (anchorDoc: Doc) (items: Doc list) : Doc =
    let subsequentIndent = defaultArg layout.subsequentItemsIndent 0

    match items with
    | [] -> anchorDoc
    | _ when layout.placeFirstItemOnNewLine -> anchorDoc <+> nest subsequentIndent (line <+> join line items)
    | _ -> anchorDoc <++> sequenceDoc layout items

let listSequenceLayout (cfg: Style) =
    let indent = cfg.whitespace.numberOfSpacesInTabs

    { placeFirstItemOnNewLine = cfg.lists.placeFirstItemOnNewLine = PlaceOnNewLine.Always
      firstItemIndent = if cfg.lists.indentListItems then Some indent else None
      subsequentItemsIndent = Some indent }

let withFirstItemIndent (indent: int) (layout: SequenceLayout) =
    { layout with
        firstItemIndent = if indent > 0 then Some indent else None }

let listCommaDecoration (cfg: Style) : SequenceDecoration =
    { commaPlacement =
        if cfg.lists.placeCommasBeforeItems then
            BeforeItems
        else
            AfterItems
      addSpaceBeforeComma = cfg.lists.addSpaceBeforeComma }

let ddlCommaDecoration: SequenceDecoration =
    { commaPlacement = AfterItems
      addSpaceBeforeComma = false }

let private decorateListItemsWith (decoration: SequenceDecoration) (items: Doc list) =
    let comma =
        if decoration.addSpaceBeforeComma then
            text " ,"
        else
            text ","

    if decoration.commaPlacement = BeforeItems then
        items |> List.mapi (fun i item -> if i = 0 then item else comma <+> item)
    else
        items
        |> List.mapi (fun i item -> if i = List.length items - 1 then item else item <+> comma)

let decorateListItems (cfg: Style) (items: Doc list) =
    decorateListItemsWith (listCommaDecoration cfg) items

let decorateDdlListItems (items: Doc list) =
    decorateListItemsWith ddlCommaDecoration items

let commaListDoc (cfg: Style) (items: Doc list) : Doc =
    sequenceDoc (listSequenceLayout cfg) (decorateListItems cfg items)

let anchoredCommaSeparatedListDoc (cfg: Style) (anchorDoc: Doc) (items: Doc list) : Doc =
    anchoredSequenceDoc (listSequenceLayout cfg) anchorDoc (decorateListItems cfg items)

let anchoredCommaSeparatedListWithLayoutDoc
    (layout: SequenceLayout)
    (cfg: Style)
    (anchorDoc: Doc)
    (items: Doc list)
    : Doc =
    anchoredSequenceDoc layout anchorDoc (decorateListItems cfg items)

let ddlProcedureParameterSequenceLayout (cfg: Style) (itemCount: int) : SequenceLayout =
    let placeOnNewLine =
        match cfg.ddl.placeFirstProcedureParameterOnNewLine with
        | DdlFirstProcedureParameterOnNewLine.Always -> true
        | DdlFirstProcedureParameterOnNewLine.IfMultipleItems -> itemCount > 1
        | _ -> false

    { placeFirstItemOnNewLine = placeOnNewLine
      firstItemIndent = None
      subsequentItemsIndent = None }

let ddlConstraintColumnSequenceLayout (cfg: Style) (itemCount: int) : SequenceLayout =
    let multilineLayout =
        { placeFirstItemOnNewLine = true
          firstItemIndent = None
          subsequentItemsIndent = None }

    let collapsedLayout =
        { placeFirstItemOnNewLine = false
          firstItemIndent = None
          subsequentItemsIndent = None }

    match cfg.ddl.placeConstraintColumnsOnNewLines with
    | DdlConstraintColumnsOnNewLines.Always -> multilineLayout
    | DdlConstraintColumnsOnNewLines.IfLongerOrMultipleColumns when itemCount > 1 -> multilineLayout
    | _ -> collapsedLayout

let ddlConstraintColumnListDoc (cfg: Style) (items: Doc list) : Doc =
    let layout = ddlConstraintColumnSequenceLayout cfg items.Length
    let doc = sequenceDoc layout (decorateDdlListItems items)

    match cfg.ddl.placeConstraintColumnsOnNewLines with
    | DdlConstraintColumnsOnNewLines.Always
    | DdlConstraintColumnsOnNewLines.IfLongerOrMultipleColumns when items.Length > 1 -> doc
    | _ -> group doc
