/// Strict pretty-printing algebra with a Lindig-style renderer.
module TSqlFormatter.Lindig

/// The document type for pretty printing.
[<RequireQualifiedAccess>]
type Doc =
    private
    | Nil
    | Cons of Doc * Doc
    | Text of string
    | Nest of int * Doc
    | Break of string
    | Group of Doc

type private SDoc =
    | SNil
    | SText of string * SDoc
    | SLine of int * SDoc

type private Mode =
    | Flat
    | Break

let empty = Doc.Nil

let text (s: string) =
    if s.Length = 0 then Doc.Nil else Doc.Text s

let line = Doc.Break " "

let linebreak = Doc.Break ""

let (<+>) (a: Doc) (b: Doc) : Doc =
    match a, b with
    | Doc.Nil, _ -> b
    | _, Doc.Nil -> a
    | _ -> Doc.Cons(a, b)

let (<++>) a b = a <+> text " " <+> b

let (<++?>) a =
    function
    | Doc.Nil -> a
    | b -> a <++> b

let (</>) a b = a <+> line <+> b

let nest (indent: int) (doc: Doc) : Doc =
    match doc with
    | Doc.Nil -> Doc.Nil
    | _ -> Doc.Nest(indent, doc)

let group (doc: Doc) : Doc = Doc.Group doc

let softline = group line

let softbreak = group linebreak

let private fits (width: int) (items: (int * Mode * Doc) list) : bool =
    let rec go w docs =
        if w < 0 then
            false
        else
            match docs with
            | [] -> true
            | (_, _, Doc.Nil) :: rest -> go w rest
            | (_, _, Doc.Text s) :: rest -> go (w - s.Length) rest
            | (i, m, Doc.Cons(a, b)) :: rest -> go w ((i, m, a) :: (i, m, b) :: rest)
            | (i, m, Doc.Nest(j, d)) :: rest -> go w ((i + j, m, d) :: rest)
            | (_, Flat, Doc.Break s) :: rest -> go (w - s.Length) rest
            | (_, Break, Doc.Break _) :: _ -> true
            | (i, Flat, Doc.Group d) :: rest -> go w ((i, Flat, d) :: rest)
            | (i, Break, Doc.Group d) :: rest ->
                if go w ((i, Flat, d) :: rest) then
                    true
                else
                    go w ((i, Break, d) :: rest)

    go width items

let private best (width: int) (doc: Doc) : SDoc =
    let rec go (col: int) (items: (int * Mode * Doc) list) : SDoc =
        match items with
        | [] -> SNil
        | (_, _, Doc.Nil) :: rest -> go col rest
        | (_, _, Doc.Text s) :: rest -> SText(s, go (col + s.Length) rest)
        | (i, m, Doc.Cons(a, b)) :: rest -> go col ((i, m, a) :: (i, m, b) :: rest)
        | (i, m, Doc.Nest(j, d)) :: rest -> go col ((i + j, m, d) :: rest)
        | (_, Flat, Doc.Break s) :: rest -> SText(s, go (col + s.Length) rest)
        | (i, Break, Doc.Break _) :: rest -> SLine(i, go i rest)
        | (i, Flat, Doc.Group d) :: rest -> go col ((i, Flat, d) :: rest)
        | (i, Break, Doc.Group d) :: rest ->
            if fits (width - col) ((i, Flat, d) :: rest) then
                go col ((i, Flat, d) :: rest)
            else
                go col ((i, Break, d) :: rest)

    go 0 [ (0, Break, doc) ]

let render (maxWidth: int) (doc: Doc) : string =
    let sb = System.Text.StringBuilder()

    let rec go pendingIndent =
        function
        | SNil -> ()
        | SText(s, d) ->
            match pendingIndent with
            | Some indent when indent > 0 -> sb.Append(System.String(' ', indent)) |> ignore
            | _ -> ()

            sb.Append(s) |> ignore
            go None d
        | SLine(indent, d) ->
            sb.AppendLine() |> ignore
            go (Some indent) d

    go None (best maxWidth doc)
    sb.ToString().TrimEnd()

let join (sep: Doc) (docs: Doc list) : Doc =
    match docs with
    | [] -> empty
    | [ d ] -> d
    | d :: rest -> List.fold (fun acc x -> acc <+> sep <+> x) d rest

let vcat (docs: Doc list) : Doc = join line docs

let hcat (docs: Doc list) : Doc = join (text " ") docs

let commaSep (docs: Doc list) : Doc =
    match docs with
    | [] -> empty
    | [ d ] -> d
    | d :: rest -> List.fold (fun acc x -> acc <+> text "," <+> line <+> x) d rest
