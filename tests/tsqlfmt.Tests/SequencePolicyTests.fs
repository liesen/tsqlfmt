module SequenceLayoutPolicyTests

open Xunit
open TSqlFormatter.Style
open TSqlFormatter.Formatter
open TSqlFormatter.Lists
open TestSupport

[<Fact>]
let ``andOrSequenceLayout uses indented alignment from style`` () =
    let testConfig =
        { config with
            operators =
                { config.operators with
                    andOr =
                        { config.operators.andOr with
                            alignment = Alignment.Indented } } }

    let layout = andOrSequenceLayout testConfig
    Assert.Equal(None, layout.firstItemIndent)
    Assert.Equal(Some 4, layout.subsequentItemsIndent)

[<Fact>]
let ``andOrSequenceLayout leaves subsequent items unindented for left alignment`` () =
    let testConfig =
        { config with
            operators =
                { config.operators with
                    andOr =
                        { config.operators.andOr with
                            alignment = Alignment.LeftAligned } } }

    let layout = andOrSequenceLayout testConfig
    Assert.Equal(None, layout.firstItemIndent)
    Assert.Equal(None, layout.subsequentItemsIndent)

[<Fact>]
let ``listSequenceLayout respects first item and indent settings`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true } }

    let layout = listSequenceLayout testConfig
    Assert.Equal(Some 4, layout.firstItemIndent)
    Assert.Equal(Some 4, layout.subsequentItemsIndent)

[<Fact>]
let ``withFirstItemIndent adds a first item indent without changing subsequent indentation`` () =
    let baseLayout = andOrSequenceLayout config
    let updated = withFirstItemIndent 4 baseLayout
    Assert.Equal(Some 4, updated.firstItemIndent)
    Assert.Equal(baseLayout.subsequentItemsIndent, updated.subsequentItemsIndent)
    Assert.Equal(baseLayout.placeFirstItemOnNewLine, updated.placeFirstItemOnNewLine)

[<Fact>]
let ``withFirstItemIndent removes indent when given zero`` () =
    let baseLayout =
        { andOrSequenceLayout config with
            firstItemIndent = Some 4 }

    let updated = withFirstItemIndent 0 baseLayout
    Assert.Equal(None, updated.firstItemIndent)
