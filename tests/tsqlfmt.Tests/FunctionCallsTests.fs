module FunctionCallsTests

open Xunit
open TSqlFormatter.Doc
open TSqlFormatter.FunctionCalls
open TSqlFormatter.Style
open TestSupport

[<Fact>]
let ``callDoc renders default flat function-call arguments`` () =
    let doc =
        callDoc config (text "DATEADD") [ text "day"; text "-90"; text "GETUTCDATE()" ]

    assertRenderedDoc config.whitespace.wrapLinesLongerThan "DATEADD(day, -90, GETUTCDATE())" doc

[<Fact>]
let ``callDoc renders multiline arguments when configured always`` () =
    let testConfig =
        { config with
            functionCalls =
                { config.functionCalls with
                    placeArgumentsOnNewLines = PlaceOnNewLine.Always } }

    let doc =
        callDoc testConfig (text "DATEADD") [ text "day"; text "-90"; text "GETUTCDATE()" ]

    let expected =
        """
DATEADD(
    day,
    -90,
    GETUTCDATE()
)
"""

    assertRenderedDoc testConfig.whitespace.wrapLinesLongerThan expected doc

[<Fact>]
let ``callDoc renders empty parentheses with configured inner space`` () =
    let testConfig =
        { config with
            functionCalls =
                { config.functionCalls with
                    addSpaceBetweenEmptyParentheses = true
                    placeArgumentsOnNewLines = PlaceOnNewLine.Never } }

    let doc = callDoc testConfig (text "GETDATE") []

    assertRenderedDoc testConfig.whitespace.wrapLinesLongerThan "GETDATE( )" doc
