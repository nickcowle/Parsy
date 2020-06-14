namespace Parsy.Test

open FsCheck
open Parsy
open Xunit

[<RequireQualifiedAccess>]
module TestTextParserReducer =

    let rec times n f a =
        match n with
        | 0 -> a
        | _ -> times (n - 1) f (f a)

    [<Fact>]
    let ``A reduced TextParser returns the same parses as the original`` () =
        let prop (ParserAndSampleInput (parser, input)) =
            let makeParser = TestTextParserUtils.makeParser ParserType.ReferenceTextParser
            let reduced = parser |> TextParserReducer.reduce
            makeParser parser input = makeParser reduced input
        TestTextParserUtils.check prop

    [<Fact>]
    let ``oneOrMores wrapping a fail are reduced away`` () =
        let prop (NonNegativeInt n) =
            let original = TextParser.fail |> times n TextParser.oneOrMore
            let reduced = TextParserReducer.reduce original
            TextParserUtils.tryCompare reduced TextParser.fail = Some true
        TestTextParserUtils.check prop

    [<Fact>]
    let ``filters wrapping a fail are reduced away`` () =
        let prop (NonNegativeInt n) =
            let original = TextParser.fail |> times n (TextParser.filter (fun _ -> failwith ""))
            let reduced = TextParserReducer.reduce original
            TextParserUtils.tryCompare reduced TextParser.fail = Some true
        TestTextParserUtils.check prop

    [<Fact>]
    let ``example reduce of bind, choice, success and fail`` () =
        let prop (NonNull s) =
            let inner = TextParser.string s
            let original = TextParser.bind (fun _ -> inner) (TextParser.choice [ TextParser.success ; TextParser.fail ])
            let reduced = TextParserReducer.reduce original
            TextParserUtils.tryCompare reduced inner = Some true
        TestTextParserUtils.check prop
