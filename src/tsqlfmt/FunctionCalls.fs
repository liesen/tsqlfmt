module TSqlFormatter.FunctionCalls

open TSqlFormatter.Doc
open TSqlFormatter.Style

let callDoc (cfg: Style) (functionNameDoc: Doc) (argDocs: Doc list) =
    let openParen, closeParen =
        if cfg.functionCalls.addSpacesAroundParentheses then
            text " (", text ")"
        else
            text "(", text ")"

    let argsDoc =
        match argDocs with
        | [] when cfg.functionCalls.addSpaceBetweenEmptyParentheses -> openParen <++> text ")"
        | [] -> openParen <+> text ")"
        | _ -> 
            let ``open``, close =
                if cfg.functionCalls.addSpacesAroundArgumentList then
                    text " " <+> openParen, closeParen <+> text " "
                else
                    openParen, closeParen
            
            if cfg.functionCalls.placeArgumentsOnNewLines = PlaceOnNewLine.Never then
                ``open`` <+> join (text ", ") argDocs <+> close
            else
                let argListSpacing = if cfg.functionCalls.addSpacesAroundArgumentList then line else softline
                let doc = ``open`` <+> nest cfg.whitespace.numberOfSpacesInTabs (argListSpacing <+> join (text "," <+> line) argDocs) <+> argListSpacing <+> close
                
                match cfg.functionCalls.placeArgumentsOnNewLines with
                | PlaceOnNewLine.Always -> doc
                | _ -> group doc

    functionNameDoc <+> argsDoc
