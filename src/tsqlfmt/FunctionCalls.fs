module TSqlFormatter.FunctionCalls

open TSqlFormatter.Doc
open TSqlFormatter.Style

let callDoc (cfg: Style) (functionNameDoc: Doc) (argDocs: Doc list) =
    let openDoc =
        if cfg.functionCalls.addSpacesAroundParentheses then
            text " ("
        else
            text "("

    let argsDoc =
        match argDocs with
        | [] when cfg.functionCalls.addSpaceBetweenEmptyParentheses -> openDoc <++> text ")"
        | [] -> openDoc <+> text ")"
        | _ ->
            let openDoc, closeDoc =
                if cfg.functionCalls.addSpacesAroundArgumentList then
                    openDoc <+> text " ", text " )"
                else
                    openDoc, text ")"

            if cfg.functionCalls.placeArgumentsOnNewLines = PlaceOnNewLine.Never then
                openDoc <+> join (text ", ") argDocs <+> closeDoc
            else
                let argListSpacing =
                    if cfg.functionCalls.addSpacesAroundArgumentList then
                        line
                    else
                        softline

                let doc =
                    openDoc
                    <+> nest cfg.whitespace.numberOfSpacesInTabs (argListSpacing <+> join (text "," <+> line) argDocs)
                    <+> argListSpacing
                    <+> closeDoc

                match cfg.functionCalls.placeArgumentsOnNewLines with
                | PlaceOnNewLine.Always -> doc
                | _ -> group doc

    functionNameDoc <+> argsDoc
