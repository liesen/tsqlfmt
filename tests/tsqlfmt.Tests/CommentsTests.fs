module CommentsTests

open Xunit
open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Trivia
open TestSupport

[<Fact>]
let ``splitCommaInterComments finds comments before and after the comma`` () =
    let stmt = parseSelectStatement "SELECT a /* before */, /* after */ b"
    let spec = stmt.QueryExpression :?> QuerySpecification
    let items = spec.SelectElements |> Seq.toList

    let beforeComma, afterComma, sawNewlineAfterComma =
        splitCommaInterComments items.[0] items.[1]

    Assert.Equal<Comment list>([ MultilineComment "/* before */" ], beforeComma)
    Assert.Equal<Comment list>([ MultilineComment "/* after */" ], afterComma)
    Assert.False(sawNewlineAfterComma)

[<Fact>]
let ``splitCommaInterComments detects newline after the comma`` () =
    let stmt =
        parseSelectStatement
            "SELECT a, /* after */
b"

    let spec = stmt.QueryExpression :?> QuerySpecification
    let items = spec.SelectElements |> Seq.toList

    let beforeComma, afterComma, sawNewlineAfterComma =
        splitCommaInterComments items.[0] items.[1]

    Assert.Empty(beforeComma)
    Assert.Equal<Comment list>([ MultilineComment "/* after */" ], afterComma)
    Assert.True(sawNewlineAfterComma)

[<Fact>]
let ``splitBooleanInterComments finds comments before and after the operator`` () =
    let expr = parseBooleanExpression "a = 1 /* before and */ AND /* after and */ b = 2"
    let bb = expr :?> BooleanBinaryExpression
    let beforeOp, afterOp = splitBooleanInterComments bb

    Assert.Equal<Comment list>([ MultilineComment "/* before and */" ], beforeOp)
    Assert.Equal<Comment list>([ MultilineComment "/* after and */" ], afterOp)

[<Fact>]
let ``trailingTriviaAfterFragment returns multiline comment after select element`` () =
    let stmt = parseSelectStatement "SELECT a /* trailing */"
    let spec = stmt.QueryExpression :?> QuerySpecification
    let elem = spec.SelectElements.[0]

    let trivia = trailingTriviaAfterFragment elem

    Assert.Equal<Comment option>(Some(MultilineComment "/* trailing */"), trivia)
