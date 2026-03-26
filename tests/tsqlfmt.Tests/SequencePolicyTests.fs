module SequencePolicyTests

open Xunit
open TSqlFormatter.Style
open TSqlFormatter.Formatter
open TSqlFormatter.Lists
open TestSupport

[<Fact>]
let ``andOrSequencePolicy uses indented alignment from style`` () =
    let testConfig =
        { config with
            operators =
                { config.operators with
                    andOr =
                        { config.operators.andOr with
                            alignment = Alignment.Indented } } }

    let policy = andOrSequencePolicy testConfig
    Assert.Equal(None, policy.firstItemIndent)
    Assert.Equal(Some 4, policy.subsequentItemsIndent)

[<Fact>]
let ``andOrSequencePolicy leaves subsequent items unindented for left alignment`` () =
    let testConfig =
        { config with
            operators =
                { config.operators with
                    andOr =
                        { config.operators.andOr with
                            alignment = Alignment.LeftAligned } } }

    let policy = andOrSequencePolicy testConfig
    Assert.Equal(None, policy.firstItemIndent)
    Assert.Equal(None, policy.subsequentItemsIndent)

[<Fact>]
let ``listSequencePolicy respects first item and indent settings`` () =
    let testConfig =
        { config with
            lists =
                { config.lists with
                    placeFirstItemOnNewLine = PlaceOnNewLine.Always
                    indentListItems = true } }

    let policy = listSequencePolicy testConfig
    Assert.Equal(Some 4, policy.firstItemIndent)
    Assert.Equal(Some 4, policy.subsequentItemsIndent)

[<Fact>]
let ``withFirstItemIndent adds a first item indent without changing subsequent indentation`` () =
    let basePolicy = andOrSequencePolicy config
    let updated = withFirstItemIndent 4 basePolicy
    Assert.Equal(Some 4, updated.firstItemIndent)
    Assert.Equal(basePolicy.subsequentItemsIndent, updated.subsequentItemsIndent)
    Assert.Equal(basePolicy.placeFirstItemOnNewLine, updated.placeFirstItemOnNewLine)

[<Fact>]
let ``withFirstItemIndent removes indent when given zero`` () =
    let basePolicy =
        { andOrSequencePolicy config with
            firstItemIndent = Some 4 }

    let updated = withFirstItemIndent 0 basePolicy
    Assert.Equal(None, updated.firstItemIndent)
