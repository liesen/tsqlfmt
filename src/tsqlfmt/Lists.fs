module TSqlFormatter.Lists

open TSqlFormatter.Doc
open TSqlFormatter.Style

type SequencePolicy =
    { placeFirstItemOnNewLine: bool
      firstItemIndent: int option
      subsequentItemsIndent: int option }

let sequenceDoc (policy: SequencePolicy) (items: Doc list) : Doc =
    let nestIfIndented indent doc =
        match indent with
        | Some spaces -> nest spaces doc
        | None -> doc

    let subsequentIndent = defaultArg policy.subsequentItemsIndent 0

    match items with
    | [] -> empty
    | [ singleItem ] -> nestIfIndented policy.firstItemIndent singleItem
    | firstItem :: remainingItems ->
        let firstItemDoc = nestIfIndented policy.firstItemIndent firstItem
        let remainingItemsDoc = join line remainingItems
        firstItemDoc <+> nest subsequentIndent (line <+> remainingItemsDoc)

let headedSequenceDoc (policy: SequencePolicy) (headDoc: Doc) (items: Doc list) : Doc =
    let subsequentIndent = defaultArg policy.subsequentItemsIndent 0

    match items with
    | [] -> headDoc
    | _ when policy.placeFirstItemOnNewLine -> headDoc <+> nest subsequentIndent (line <+> join line items)
    | _ -> headDoc <++> sequenceDoc policy items

let listSequencePolicy (cfg: Style) =
    let indent = cfg.whitespace.numberOfSpacesInTabs

    { placeFirstItemOnNewLine = cfg.lists.placeFirstItemOnNewLine = PlaceOnNewLine.Always
      firstItemIndent = if cfg.lists.indentListItems then Some indent else None
      subsequentItemsIndent = Some indent }

let withFirstItemIndent (indent: int) (policy: SequencePolicy) =
    { policy with
        firstItemIndent = if indent > 0 then Some indent else None }

let private commaItems (cfg: Style) (items: Doc list) =
    let comma =
        if cfg.lists.addSpaceBeforeComma then
            text " ,"
        else
            text ","

    if cfg.lists.placeCommasBeforeItems then
        items |> List.mapi (fun i item -> if i = 0 then item else comma <+> item)
    else
        items
        |> List.mapi (fun i item -> if i = List.length items - 1 then item else item <+> comma)

let commaListDoc (cfg: Style) (items: Doc list) : Doc =
    sequenceDoc (listSequencePolicy cfg) (commaItems cfg items)
