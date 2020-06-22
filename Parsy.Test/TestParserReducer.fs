namespace Parsy.Test

open Parsy
open Xunit

[<RequireQualifiedAccess>]
module TestParserReducer =

    [<Fact>]
    let ``A reduced Parser returns the same parses as the original`` () =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            let makeParser = TestUtils.makeParser ParserType.ReferenceParser
            let reduced = parser |> ParserReducer.reduce
            makeParser parser input = makeParser reduced input
        TestUtils.check prop
