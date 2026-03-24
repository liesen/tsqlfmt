module TSqlFormatter.Identifiers

open Microsoft.SqlServer.TransactSql.ScriptDom
open TSqlFormatter.Doc

let identDoc (ident: Identifier) : Doc =
    match ident.QuoteType with
    | QuoteType.SquareBracket -> text ("[" + ident.Value + "]")
    | QuoteType.DoubleQuote -> text ("\"" + ident.Value + "\"")
    | _ -> text ident.Value

let identOrValueDoc (iov: IdentifierOrValueExpression) : Doc =
    if iov.Identifier <> null then
        identDoc iov.Identifier
    else
        text ("'" + iov.Value.Replace("'", "''") + "'")

let multiPartIdentDoc (mpi: MultiPartIdentifier) : Doc =
    mpi.Identifiers |> Seq.map identDoc |> Seq.toList |> join (text ".")

let schemaObjectNameDoc (son: SchemaObjectName) : Doc =
    son.Identifiers |> Seq.map identDoc |> Seq.toList |> join (text ".")
