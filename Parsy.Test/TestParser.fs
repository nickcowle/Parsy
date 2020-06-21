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

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``mapping a parser is equivalent to mapping its parses`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser : int Parser, input)) (MappingFunction f) =
            let actualParsed = makeParser parserType (Parser.map f parser) input
            let expectedParsed = makeParser parserType parser input |> Set.map (fun (a, segment) -> f a, segment)
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``bind behaves in the same way as sequence, for a fixed second parser`` (parserType : ParserType) =
        let prop (ParserAndSampleInput (parser1 : int Parser, input1)) (ParserAndSampleInput (parser2 : int Parser, input2)) =
            let input = input1 + input2
            let actualParsed = makeParser parserType (Parser.bind (fun _ -> parser2) parser1) input
            let expectedParsed = makeParser parserType (Parser.sequence (fun _ b -> b) parser1 parser2) input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``zeroOrMore returns all n+1 possible parses when parsing a string repeated n times`` (parserType : ParserType) =
        let prop (NonEmptyString s) (SmallInt n) =
            let parser = TextParser.string s |> Parser.ofTextParser |> Parser.zeroOrMore "" (+) |> makeParser parserType
            let input = String.replicate n s
            let expectedParsed = [0..n] |> List.map (fun i -> String.replicate i s, String.replicate (n - i) s) |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``zeroOrMore fail is equivalent to success`` (parserType : ParserType) =
        let prop (NonNull input) (i : int) =
            let expectedParser = Parser.success i |> makeParser parserType
            let ignored _ = failwith "should never be called"
            let actualParser = Parser.zeroOrMore i ignored Parser.fail |> makeParser parserType
            expectedParser input = actualParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``oneOrMore returns all n possible parses when parsing a string repeated n times`` (parserType : ParserType) =
        let prop (NonEmptyString s) (SmallInt n) =
            let parser = TextParser.string s |> Parser.ofTextParser |> Parser.oneOrMore id (+) |> makeParser parserType
            let input = String.replicate n s
            let expectedParsed = [1..n] |> List.map (fun i -> String.replicate i s, String.replicate (n - i) s) |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``oneOrMore fail is equivalent to fail`` (parserType : ParserType) =
        let prop (NonNull input) =
            let expectedParser = Parser.fail |> makeParser parserType
            let ignored _ = failwith "should never be called"
            let actualParser = Parser.oneOrMore ignored ignored Parser.fail |> makeParser parserType
            expectedParser input = actualParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``interleave returns all n+1 possible parses when parsing two strings repeated n times`` (parserType : ParserType) =
        let prop (NonEmptyString s1) (NonEmptyString s2) (SmallInt n) =
            let string = TextParser.string >> Parser.ofTextParser
            let parser = Parser.interleave id (fun s s2 s1 -> s + s2 + s1) (string s1) (string s2) |> makeParser parserType
            let tail n = sprintf "%s%s" s2 s1 |> String.replicate n
            let head s = sprintf "%s%s" s1 s
            let input = head (tail n)
            let expectedParse i = head (tail i), tail (n - i)
            let expectedParsed = [0..n] |> List.map expectedParse |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``interleave1 returns all n possible parses when parsing two strings repeated n times`` (parserType : ParserType) =
        let prop (NonEmptyString s1) (NonEmptyString s2) (SmallInt n) =
            let string = TextParser.string >> Parser.ofTextParser
            let parser = Parser.interleave1 (fun s11 s2 s12 -> s11 + s2 + s12) (fun s s2 s1 -> s + s2 + s1) (string s1) (string s2) |> makeParser parserType
            let tail n = sprintf "%s%s" s2 s1 |> String.replicate n
            let head s = sprintf "%s%s" s1 s
            let input = head (tail n)
            let expectedParse i = head (tail i), tail (n - i)
            let expectedParsed = [1..n] |> List.map expectedParse |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Fact>]
    let ``ReferenceParser and OptimisedParser return the same parses`` () =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            makeParser ParserType.ReferenceParser parser input = makeParser ParserType.OptimisedParser parser input
        check prop

    [<Fact>]
    let ``All parses of OptimisedParser are initial segments of the input`` () =
        let prop (ParserAndSampleInput (parser : int Parser, input)) =
            let parses = ResizeArray ()
            let segment = input |> StringSegment.ofString
            OptimisedParser.make parser (fun _ -> parses.Add) segment
            parses |> Seq.forall (fun segment -> segment.Offset = 0)
        check prop

    [<Fact>]
    let ``OptimisedParser can begin parsing from the middle of a StringSegment correctly`` () =
        let prop (ParserAndSampleInput (parser : int Parser, input)) (NonNull s) =

            let parse segment =
                let parses = ResizeArray ()
                OptimisedParser.make parser (fun _ -> parses.Add) segment
                parses
                |> Seq.map (fun segment -> segment |> StringSegment.current, segment |> StringSegment.remaining)
                |> Set.ofSeq

            let expected = input |> StringSegment.ofString |> parse
            let actual = (s + input) |> StringSegment.ofString |> StringSegment.advance s.Length |> parse
            expected = actual
        check prop
