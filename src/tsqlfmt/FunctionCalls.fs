module TSqlFormatter.FunctionCalls

open TSqlFormatter.Doc
open TSqlFormatter.Style

let callDoc (cfg: Style) (functionNameDoc: Doc) (argDocs: Doc list) =
    let openParen =
        if cfg.functionCalls.addSpacesAroundParentheses then
            text " ("
        else
            text "("

    let argsDoc =
        match argDocs with
        | [] when cfg.functionCalls.addSpaceBetweenEmptyParentheses -> openParen <++> text ")"
        | [] -> openParen <+> text ")"
        | _ ->
            let ``open``, close =
                if cfg.functionCalls.addSpacesAroundArgumentList then
                    openParen <+> text " ", text " )"
                else
                    openParen, text ")"

            if cfg.functionCalls.placeArgumentsOnNewLines = PlaceOnNewLine.Never then
                ``open`` <+> join (text ", ") argDocs <+> close
            else
                let argListSpacing =
                    if cfg.functionCalls.addSpacesAroundArgumentList then
                        line
                    else
                        softline

                let doc =
                    ``open``
                    <+> nest cfg.whitespace.numberOfSpacesInTabs (argListSpacing <+> join (text "," <+> line) argDocs)
                    <+> argListSpacing
                    <+> close

                match cfg.functionCalls.placeArgumentsOnNewLines with
                | PlaceOnNewLine.Always -> doc
                | _ -> group doc

    functionNameDoc <+> argsDoc
