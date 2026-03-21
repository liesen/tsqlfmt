module TSqlFormatter.Trivia

open System.Collections.Generic
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc

let interComments (prevFrag: TSqlFragment) (nextFrag: TSqlFragment) : string list =
    if prevFrag = null || nextFrag = null then []
    else
        let stream = prevFrag.ScriptTokenStream
        if stream = null || nextFrag.ScriptTokenStream = null then []
        elif not (System.Object.ReferenceEquals(stream, nextFrag.ScriptTokenStream)) then []
        else
            [ for i in prevFrag.LastTokenIndex + 1 .. nextFrag.FirstTokenIndex - 1 do
                let tok = stream.[i]
                match tok.TokenType with
                | TSqlTokenType.SingleLineComment
                | TSqlTokenType.MultilineComment ->
                    yield tok.Text.TrimEnd()
                | _ -> () ]

let tokensInRange (stream: IList<TSqlParserToken>) (startIdx: int) (endIdx: int) : TSqlParserToken list =
    if stream = null || endIdx < startIdx then []
    else [ for i in startIdx .. endIdx -> stream.[i] ]

let ownLineComments (tokens: TSqlParserToken list) : string list =
    tokens
    |> List.fold (fun (seenNewLine, comments) tok ->
        match tok.TokenType with
        | TSqlTokenType.WhiteSpace when tok.Text.Contains('\n') || tok.Text.Contains('\r') ->
            true, comments
        | TSqlTokenType.SingleLineComment
        | TSqlTokenType.MultilineComment when seenNewLine ->
            seenNewLine, tok.Text.TrimEnd() :: comments
        | _ ->
            seenNewLine, comments
    ) (false, [])
    |> snd
    |> List.rev

let leadingInterComments (prevFrag: TSqlFragment) (nextFrag: TSqlFragment) : string list =
    if prevFrag = null || nextFrag = null then []
    else
        let stream = prevFrag.ScriptTokenStream
        if stream = null || nextFrag.ScriptTokenStream = null then []
        elif not (System.Object.ReferenceEquals(stream, nextFrag.ScriptTokenStream)) then []
        else tokensInRange stream (prevFrag.LastTokenIndex + 1) (nextFrag.FirstTokenIndex - 1) |> ownLineComments

let ownLineCommentsInRange (stream: IList<TSqlParserToken>) (startIdx: int) (endIdx: int) : string list =
    tokensInRange stream startIdx endIdx |> ownLineComments

let splitBooleanInterComments (bb: BooleanBinaryExpression) : string list * string list =
    let stream = bb.ScriptTokenStream
    if stream = null || bb.FirstExpression = null || bb.SecondExpression = null then
        [], []
    else
        tokensInRange stream (bb.FirstExpression.LastTokenIndex + 1) (bb.SecondExpression.FirstTokenIndex - 1)
        |> List.fold (fun (seenOperator, beforeOp, afterOp) tok ->
            match tok.TokenType with
            | TSqlTokenType.And
            | TSqlTokenType.Or ->
                true, beforeOp, afterOp
            | TSqlTokenType.SingleLineComment
            | TSqlTokenType.MultilineComment ->
                if seenOperator then
                    true, beforeOp, tok.Text.TrimEnd() :: afterOp
                else
                    false, tok.Text.TrimEnd() :: beforeOp, afterOp
            | _ ->
                seenOperator, beforeOp, afterOp
        ) (false, [], [])
        |> fun (_, beforeOp, afterOp) -> List.rev beforeOp, List.rev afterOp

let attachOwnLineComments (comments: string list) (doc: Doc) : Doc =
    match comments with
    | [] -> doc
    | _ -> join line (comments |> List.map text) <+> line <+> doc

let attachInlineLeadingComments (comments: string list) (doc: Doc) : Doc =
    match comments with
    | [] -> doc
    | _ -> join (text " ") (comments |> List.map text) <++> doc

let renderItemsWithLeadingComments<'T when 'T :> TSqlFragment>
    (render: 'T -> Doc)
    (leadingComments: 'T option -> 'T -> string list)
    (attachComments: string list -> Doc -> Doc)
    (items: 'T list)
    : Doc list =
    items
    |> List.mapFold (fun prev item ->
        let doc = render item |> attachComments (leadingComments prev item)
        doc, Some item
    ) None
    |> fst

let hasTrailingSemicolon (frag: TSqlFragment) =
    let stream = frag.ScriptTokenStream
    stream <> null
    && frag.LastTokenIndex >= frag.FirstTokenIndex
    && stream.[frag.LastTokenIndex].TokenType = TSqlTokenType.Semicolon
