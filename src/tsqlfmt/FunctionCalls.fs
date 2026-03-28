module TSqlFormatter.FunctionCalls

open TSqlFormatter.Doc
open TSqlFormatter.Parenthesis
open TSqlFormatter.Style

let callDoc (cfg: Style) (functionNameDoc: Doc) (argDocs: Doc list) =
    functionNameDoc <+> functionCallParensDoc cfg argDocs
