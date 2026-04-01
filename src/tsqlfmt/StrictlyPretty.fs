/// Strict pretty-printing algebra with a Strictly Pretty renderer.
/// https://lindig.github.io/papers/strictly-pretty-2000.pdf
module TSqlFormatter.StrictlyPretty

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

// Simple document for rendering
type private SDoc =
    | SNil
    | SText of string * SDoc
    | SLine of int * SDoc

let rec private sdocToString =
    function
    | SNil -> ""
    | SText(s, d) -> s + sdocToString d
    | SLine(i, d) -> let prefix = String.replicate i " " in "\n" + prefix + sdocToString d

let join (sep: Doc) : Doc list -> Doc =
    List.reduce (fun acc x -> acc <+> sep <+> x)

let vcat = join line

let hcat = join (text " ")

type private Mode =
    | Flat
    | Break

let rec private fits w =
    function
    | _ when w < 0 -> false
    | [] -> true
    | (i, m, Doc.Nil) :: z -> fits w z
    | (i, m, Doc.Cons(x, y)) :: z -> fits w ((i, m, x) :: (i, m, y) :: z)
    | (i, m, Doc.Nest(j, x)) :: z -> fits w ((i + j, m, x) :: z)
    | (i, m, Doc.Text(s)) :: z -> fits (w - s.Length) z
    | (i, Flat, Doc.Break(s)) :: z -> fits (w - s.Length) z
    | (i, Break, Doc.Break(_)) :: z -> true (* impossible *)
    | (i, m, Doc.Group(x)) :: z -> fits w ((i, Flat, x) :: z)

let rec private format w k =
    function
    | [] -> SNil
    | (i, m, Doc.Nil) :: z -> format w k z
    | (i, m, Doc.Cons(x, y)) :: z -> format w k ((i, m, x) :: (i, m, y) :: z)
    | (i, m, Doc.Nest(j, x)) :: z -> format w k ((i + j, m, x) :: z)
    | (i, m, Doc.Text(s)) :: z -> SText(s, format w (k + s.Length) z)
    | (i, Flat, Doc.Break(s)) :: z -> SText(s, format w (k + s.Length) z)
    | (i, Break, Doc.Break(s)) :: z -> SLine(i, format w i z)
    | (i, m, Doc.Group(x)) :: z ->
        if fits (w - k) ((i, Flat, x) :: z) then
            format w k ((i, Flat, x) :: z)
        else
            format w k ((i, Break, x) :: z)

let render (maxWidth: int) (doc: Doc) : string =
    let sdoc = format maxWidth 0 [ (0, Break, doc) ]

    // Render SDoc but trim trailing whitespace
    let sb = System.Text.StringBuilder()

    let rec go pendingIndent =
        function
        | SNil -> ()
        | SText(s, d) when s.Length = 0 -> go pendingIndent d
        | SText(s, d) ->
            match pendingIndent with
            | Some indent when indent > 0 -> sb.Append(System.String(' ', indent)) |> ignore
            | _ -> ()

            sb.Append(s) |> ignore
            go None d
        | SLine(indent, d) ->
            sb.AppendLine() |> ignore
            go (Some indent) d

    go None sdoc
    sb.ToString().TrimEnd()
