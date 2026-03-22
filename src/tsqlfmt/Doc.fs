/// Wadler's "A Prettier Printer" algebra implementation.
/// Standard implementation with Cat and Nest constructors.
module TSqlFormatter.Doc

/// The document type for pretty printing.
[<RequireQualifiedAccess>]
type Doc =
    | Nil
    | Text of string
    | Line // Line break, renders at current indent
    | Cat of Doc * Doc // Concatenation
    | Nest of int * Doc // Add indent to context for doc
    | Union of Doc * Doc // Flattened vs normal

/// Simple document for rendering.
type SDoc =
    | SNil
    | SText of string * SDoc
    | SLine of int * SDoc

// ─── Constructors ───

let empty = Doc.Nil

let text (s: string) =
    if s.Length = 0 then Doc.Nil else Doc.Text(s)

let line = Doc.Line

// ─── Concatenation ───

/// Concatenation operator
let (<+>) (a: Doc) (b: Doc) : Doc =
    match a, b with
    | Doc.Nil, _ -> b
    | _, Doc.Nil -> a
    | _ -> Doc.Cat(a, b)

/// Concatenation with a space
let (<++>) a b = a <+> text " " <+> b

/// Concatenation with a line (soft break)
let (</>) a b = a <+> line <+> b

// ─── Indentation ───

/// Nest: adds `indent` to the indentation context for all content in `doc`.
let nest (indent: int) (doc: Doc) : Doc =
    match doc with
    | Doc.Nil -> Doc.Nil
    | _ -> Doc.Nest(indent, doc)

// ─── Flattening ───

/// Replace all line breaks with single spaces.
let rec flatten (doc: Doc) : Doc =
    match doc with
    | Doc.Nil -> Doc.Nil
    | Doc.Text _ -> doc
    | Doc.Line -> Doc.Text(" ")
    | Doc.Cat(a, b) -> Doc.Cat(flatten a, flatten b)
    | Doc.Nest(i, d) -> Doc.Nest(i, flatten d)
    | Doc.Union(a, _) -> flatten a

/// `group` tries to flatten a document onto a single line.
/// If it fits, use the flattened version; otherwise keep line breaks.
let group (doc: Doc) : Doc = Doc.Union(flatten doc, doc)

// ─── Rendering ───

type private Mode =
    | Flat
    | Break

/// Check if the first line of a simple doc fits in `w` columns.
let rec private fits (w: int) (sdoc: SDoc) : bool =
    if w < 0 then
        false
    else
        match sdoc with
        | SNil -> true
        | SText(s, d) -> fits (w - s.Length) d
        | SLine _ -> true

/// Best layout for a document given current column width and indentation.
let private best (width: int) (doc: Doc) : SDoc =
    // Each item on the work list: (current_indent, mode, doc)
    let rec go (col: int) (items: (int * Mode * Doc) list) : SDoc =
        match items with
        | [] -> SNil
        | (_, _, Doc.Nil) :: rest -> go col rest
        | (i, m, Doc.Text(s)) :: rest -> SText(s, go (col + s.Length) rest)
        | (i, m, Doc.Cat(a, b)) :: rest -> go col ((i, m, a) :: (i, m, b) :: rest)
        | (i, Flat, Doc.Line) :: rest ->
            // In flat mode, line becomes a space
            SText(" ", go (col + 1) rest)
        | (i, Break, Doc.Line) :: rest ->
            // In break mode, emit a line at current indent
            SLine(i, go i rest)
        | (i, m, Doc.Nest(j, d)) :: rest ->
            // Push new indent context for the nested doc
            go col ((i + j, m, d) :: rest)
        | (i, _, Doc.Union(a, b)) :: rest ->
            let sdocA = go col ((i, Flat, a) :: rest)

            if fits (width - col) sdocA then
                sdocA
            else
                go col ((i, Break, b) :: rest)

    go 0 [ (0, Break, doc) ]

/// Render a `Doc` to a string with the given max line width.
let render (maxWidth: int) (doc: Doc) : string =
    let sb = System.Text.StringBuilder()

    let rec go =
        function
        | SNil -> ()
        | SText(s, d) ->
            sb.Append(s) |> ignore
            go d
        | SLine(indent, d) ->
            sb.AppendLine() |> ignore
            sb.Append(System.String(' ', indent)) |> ignore
            go d

    go (best maxWidth doc)
    sb.ToString().TrimEnd()

// ─── Utility combinators ───

/// Concatenate documents with a separator between each pair.
let join (sep: Doc) (docs: Doc list) : Doc =
    match docs with
    | [] -> empty
    | [ d ] -> d
    | d :: rest -> List.fold (fun acc x -> acc <+> sep <+> x) d rest

/// Concatenate documents with hardline between each.
let vcat (docs: Doc list) : Doc = join line docs

/// Concatenate documents with space between each.
let hcat (docs: Doc list) : Doc = join (text " ") docs

/// Separate docs with comma + space, with line breaks.
let commaSep (docs: Doc list) : Doc =
    match docs with
    | [] -> empty
    | [ d ] -> d
    | d :: rest -> List.fold (fun acc x -> acc <+> text "," <+> line <+> x) d rest

/// Like commaSep but uses hardline.
let commaHardSep (docs: Doc list) : Doc =
    match docs with
    | [] -> empty
    | [ d ] -> d
    | d :: rest -> List.fold (fun acc x -> acc <+> text "," <+> Doc.Line <+> x) d rest
