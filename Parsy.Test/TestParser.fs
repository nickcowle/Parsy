namespace Parsy.Test

open Parsy
open Xunit
open FsCheck

[<RequireQualifiedAccess>]
module TestParser =

    let (.+.) p1 p2 = Parser.choice [ p1 ; p2 ]

    let allParserTypesMemberData =
        [
            ReferenceParser
            OptimisedParser
        ]
        |> List.map (fun p -> [| box p |])

    [<Literal>]
    let allParserTypes = "allParserTypesMemberData"
    let check = TestUtils.check
    let makeParser = TestUtils.makeParser

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``Parsers always returns a value and some end segment of the input`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            makeParser parserType parser input |> Set.forall (fun (_, rest) -> input.EndsWith rest)
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``ofTextParser is equivalent to parsing using the TextParser itself`` (parserType : ParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let textParser = parser |> TestUtils.makeTextParser TextParserType.ReferenceTextParser
            let parser = parser |> Parser.ofTextParser |> makeParser parserType
            textParser input = parser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success returns a value and then original input`` (parserType : ParserType) =
        let prop (i : int) (NonNull s) =
            makeParser parserType (Parser.success i) s = Set.singleton (i, s)
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail returns nothing`` (parserType : ParserType) =
        let prop (s : string) =
            makeParser parserType Parser.fail s = Set.empty
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``choice always returns the result of each of the individual parsers`` (parserType : ParserType) =

        let prop (parsersAndInputs : int ParserAndSampleInput list) =

            let parser =
                parsersAndInputs
                |> List.map (fun (ParserAndSampleInput (parser, _)) -> parser)
                |> Parser.choice
                |> makeParser parserType

            let expectedOutputs =
                parsersAndInputs
                |> Seq.map (fun (ParserAndSampleInput (parser, input)) -> makeParser parserType parser input)
                |> Set.unionMany

            let parsed =
                parsersAndInputs
                |> Seq.map (fun (ParserAndSampleInput (_, input)) -> parser input)
                |> Set.unionMany

            expectedOutputs |> Set.forall (fun result -> parsed |> Set.contains result)

        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``choice of one is equivalent to the single parser`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            let actualParser = [ parser ] |> Parser.choice |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``empty choice is equivalent to fail`` (parserType : ParserType) =
        let prop (input : string) =
            let actualParser = [ ] |> Parser.choice |> makeParser parserType
            let expectedParser = Parser.fail |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a left unit of binary choice`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            let actualParser = Parser.fail .+. parser |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a right unit of binary choice`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            let actualParser = parser .+. Parser.fail |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``binary choice is commutative`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, _)) =
            let p1 = parser1 .+. parser2 |> makeParser parserType
            let p2 = parser2 .+. parser1 |> makeParser parserType
            p1 input1 = p2 input1
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``binary choice is associative`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, _)) (ParserAndSampleInput (parser3, _)) =
            let p1 = (parser1 .+. parser2) .+. parser3 |> makeParser parserType
            let p2 = parser1 .+. (parser2 .+. parser3) |> makeParser parserType
            p1 input1 = p2 input1
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence consumes the concatenation of strings consumed by its constituent parsers`` (parserType : ParserType) =

        let prop (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, input2)) =
            match makeParser parserType parser1 (input1 + input2) |> Set.toList with
            | [] ->
                let actualParsed = makeParser parserType (Parser.sequence (+) parser1 parser2) (input1 + input2)
                let expectedParsed = Set.empty
                actualParsed = expectedParsed
            | (parsed1, rest1)::_ ->
                let parsed2 = makeParser parserType parser2 input2
                let input = (input1 + input2).Substring(0, input1.Length + input2.Length - rest1.Length) + input2
                let actualParsed = makeParser parserType (Parser.sequence (+) parser1 parser2) input
                let expectedParsed = parsed2 |> Set.map (fun (parsed, rest) -> parsed1 + parsed, rest)
                expectedParsed |> Set.forall (fun expected -> actualParsed |> Set.contains expected)

        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence distributes over binary choice to the left`` (parserType : ParserType) =
        let prop
            (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, input2))
            (ParserAndSampleInput (parser3, input3)) (CombinerFunction f) =
            let (.*.) = Parser.sequence f
            let p1 = parser1 .*. (parser2 .+. parser3)
            let p2 = parser1 .*. parser2 .+. parser1 .*. parser3
            let input = input1 + input2 + input3
            makeParser parserType p1 input = makeParser parserType p2 input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence distributes over binary choice to the right`` (parserType : ParserType) =
        let prop
            (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, input2))
            (ParserAndSampleInput (parser3, input3)) (CombinerFunction f) =
            let (.*.) = Parser.sequence f
            let p1 = (parser1 .+. parser2) .*. parser3
            let p2 = parser1 .*. parser3 .+. parser2 .*. parser3
            let input = input1 + input2 + input3
            makeParser parserType p1 input = makeParser parserType p2 input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a left zero of sequence`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) (CombinerFunction f) =
            let (.*.) = Parser.sequence f
            let actualParsed = makeParser parserType (Parser.fail .*. parser) input
            let expectedParsed = Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a right zero of sequence`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) (CombinerFunction f) =
            let (.*.) = Parser.sequence f
            let actualParsed = makeParser parserType (parser .*. Parser.fail) input
            let expectedParsed = Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success is a left unit of sequence`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser, input)) (CombinerFunction f) (i : int) =
            let (.*.) = Parser.sequence f
            let actualParsed = makeParser parserType (Parser.success i .*. parser) input
            let expectedParsed = makeParser parserType (Parser.map (f i) parser) input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success is a right unit of sequence`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser, input)) (CombinerFunction f) (i : int) =
            let (.*.) = Parser.sequence f
            let actualParsed = makeParser parserType (parser .*. Parser.success i) input
            let expectedParsed = makeParser parserType (Parser.map (fun n -> f n i) parser) input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence is associative if the combiner operation is associative`` (parserType : ParserType) =
        let prop
            (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2, input2))
            (ParserAndSampleInput (parser3, input3)) (AssociativeCombinerFunction f) =
            let (.*.) = Parser.sequence f
            let p1 = (parser1 .*. parser2) .*. parser3 |> makeParser parserType
            let p2 = parser1 .*. (parser2 .*. parser3) |> makeParser parserType
            let input = input1 + input2 + input3
            p1 input = p2 input
        check prop
