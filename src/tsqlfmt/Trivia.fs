module TSqlFormatter.Trivia

open System.Collections.Generic
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc

type Comment =
    | SingleLineComment of string
    | MultilineComment of string

let interComments (prevFrag: TSqlFragment) (nextFrag: TSqlFragment) : Comment list =
    if prevFrag = null || nextFrag = null then
        []
    else
        let stream = prevFrag.ScriptTokenStream

        if stream = null || nextFrag.ScriptTokenStream = null then
            []
        elif not (System.Object.ReferenceEquals(stream, nextFrag.ScriptTokenStream)) then
            []
        else
            [ for i in prevFrag.LastTokenIndex + 1 .. nextFrag.FirstTokenIndex - 1 do
                  let tok = stream.[i]

                  match tok.TokenType with
                  | TSqlTokenType.SingleLineComment -> yield SingleLineComment(tok.Text.TrimEnd())
                  | TSqlTokenType.MultilineComment -> yield MultilineComment(tok.Text.TrimEnd())
                  | _ -> () ]

let tokensInRange (stream: IList<TSqlParserToken>) (startIdx: int) (endIdx: int) : TSqlParserToken list =
    if stream = null || endIdx < startIdx then
        []
    else
        [ for i in startIdx..endIdx -> stream.[i] ]

let ownLineComments (tokens: TSqlParserToken list) : Comment list =
    tokens
    |> List.fold
        (fun (seenNewLine, comments) tok ->
            match tok.TokenType with
            | TSqlTokenType.WhiteSpace when tok.Text.Contains('\n') || tok.Text.Contains('\r') -> true, comments
            | TSqlTokenType.SingleLineComment when seenNewLine ->
                seenNewLine, SingleLineComment(tok.Text.TrimEnd()) :: comments
            | TSqlTokenType.MultilineComment when seenNewLine ->
                seenNewLine, MultilineComment(tok.Text.TrimEnd()) :: comments
            | _ -> seenNewLine, comments)
        (false, [])
    |> snd
    |> List.rev

let leadingInterComments (prevFrag: TSqlFragment) (nextFrag: TSqlFragment) : Comment list =
    if prevFrag = null || nextFrag = null then
        []
    else
        let stream = prevFrag.ScriptTokenStream

        if stream = null || nextFrag.ScriptTokenStream = null then
            []
        elif not (System.Object.ReferenceEquals(stream, nextFrag.ScriptTokenStream)) then
            []
        else
            tokensInRange stream (prevFrag.LastTokenIndex + 1) (nextFrag.FirstTokenIndex - 1)
            |> ownLineComments

let ownLineCommentsInRange (stream: IList<TSqlParserToken>) (startIdx: int) (endIdx: int) : Comment list =
    tokensInRange stream startIdx endIdx |> ownLineComments

let splitBooleanInterComments (bb: BooleanBinaryExpression) : Comment list * Comment list =
    let stream = bb.ScriptTokenStream

    if stream = null || bb.FirstExpression = null || bb.SecondExpression = null then
        [], []
    else
        tokensInRange stream (bb.FirstExpression.LastTokenIndex + 1) (bb.SecondExpression.FirstTokenIndex - 1)
        |> List.fold
            (fun (seenOperator, beforeOp, afterOp) tok ->
                match tok.TokenType with
                | TSqlTokenType.And
                | TSqlTokenType.Or -> true, beforeOp, afterOp
                | TSqlTokenType.SingleLineComment ->
                    if seenOperator then
                        true, beforeOp, SingleLineComment(tok.Text.TrimEnd()) :: afterOp
                    else
                        false, SingleLineComment(tok.Text.TrimEnd()) :: beforeOp, afterOp
                | TSqlTokenType.MultilineComment ->
                    if seenOperator then
                        true, beforeOp, MultilineComment(tok.Text.TrimEnd()) :: afterOp
                    else
                        false, MultilineComment(tok.Text.TrimEnd()) :: beforeOp, afterOp
                | _ -> seenOperator, beforeOp, afterOp)
            (false, [], [])
        |> fun (_, beforeOp, afterOp) -> List.rev beforeOp, List.rev afterOp

let hasTrailingSemicolon (frag: TSqlFragment) =
    let stream = frag.ScriptTokenStream

    stream <> null
    && frag.LastTokenIndex >= frag.FirstTokenIndex
    && stream.[frag.LastTokenIndex].TokenType = TSqlTokenType.Semicolon

let trailingTriviaAfterTokenIndex (stream: IList<TSqlParserToken>) (lastTokenIndex: int) : Comment option =
    if stream = null then
        None
    else
        let rec scan idx =
            if idx >= stream.Count then
                None
            else
                let tok = stream.[idx]

                match tok.TokenType with
                | TSqlTokenType.WhiteSpace ->
                    if tok.Text.Contains('\n') || tok.Text.Contains('\r') then
                        None
                    else
                        scan (idx + 1)
                | TSqlTokenType.SingleLineComment -> Some(SingleLineComment(tok.Text.TrimEnd()))
                | TSqlTokenType.MultilineComment -> Some(MultilineComment tok.Text)
                | _ -> None

        scan (lastTokenIndex + 1)

let trailingCommentAfterTokenIndex (stream: IList<TSqlParserToken>) (lastTokenIndex: int) : Doc =
    match trailingTriviaAfterTokenIndex stream lastTokenIndex with
    | None -> empty
    | Some(SingleLineComment comment) -> text " " <+> text comment
    | Some(MultilineComment comment) -> text " " <+> text comment

let trailingTriviaAfterFragment (frag: TSqlFragment) : Comment option =
    if frag = null then
        None
    else
        trailingTriviaAfterTokenIndex frag.ScriptTokenStream frag.LastTokenIndex

let trailingCommentAfterFragment (frag: TSqlFragment) : Doc =
    trailingCommentAfterTokenIndex frag.ScriptTokenStream frag.LastTokenIndex

let tokenIndexOfType (frag: TSqlFragment) (tokenType: TSqlTokenType) : int option =
    let stream = frag.ScriptTokenStream

    if stream = null then
        None
    else
        seq { frag.FirstTokenIndex .. frag.LastTokenIndex }
        |> Seq.tryFind (fun i -> stream.[i].TokenType = tokenType)

let tokenIndexOfIdentifier (frag: TSqlFragment) (value: string) : int option =
    let stream = frag.ScriptTokenStream

    if stream = null then
        None
    else
        seq { frag.FirstTokenIndex .. frag.LastTokenIndex }
        |> Seq.tryFind (fun i ->
            stream.[i].TokenType = TSqlTokenType.Identifier
            && System.String.Equals(stream.[i].Text, value, System.StringComparison.OrdinalIgnoreCase))
