module FunctionCallsTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.FunctionCalls
open TestSupport

[<Fact>]
let ``callDoc renders default flat function-call arguments`` () =
    let doc =
        callDoc config (text "DATEADD") [ text "day"; text "-90"; text "GETUTCDATE()" ]

    assertRenderedDoc config.whitespace.wrapLinesLongerThan "DATEADD(day, -90, GETUTCDATE())" doc
