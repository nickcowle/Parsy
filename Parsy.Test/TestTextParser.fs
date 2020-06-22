namespace Parsy.Test

open Parsy
open Xunit
open FsCheck

[<RequireQualifiedAccess>]
module TestTextParser =

    let (.*.) = TextParser.sequence
    let (.+.) p1 p2 = TextParser.choice [ p1 ; p2 ]

    let allParserTypesMemberData =
        [
            ReferenceTextParser
            OptimisedTextParser
        ]
        |> List.map (fun p -> [| box p |])

    [<Literal>]
    let allParserTypes = "allParserTypesMemberData"
    let check = TestUtils.check
    let makeParser = TestUtils.makeTextParser

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``TextParsers only return partitions of the input`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            makeParser parserType parser input |> Set.forall (fun (parsed, rest) -> parsed + rest = input)
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success returns the empty string and then original input`` (parserType : TextParserType) =
        let prop (NonNull s) =
            makeParser parserType TextParser.success s = Set.singleton ("", s)
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail returns nothing`` (parserType : TextParserType) =
        let prop (s : string) =
            makeParser parserType TextParser.fail s = Set.empty
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``choice always returns the result of each of the individual parsers`` (parserType : TextParserType) =

        let prop (parsersAndInputs : TextParserAndSampleInput list) =

            let parser =
                parsersAndInputs
                |> List.map (fun (TextParserAndSampleInput (parser, _)) -> parser)
                |> TextParser.choice
                |> makeParser parserType

            let expectedOutputs =
                parsersAndInputs
                |> Seq.map (fun (TextParserAndSampleInput (parser, input)) -> makeParser parserType parser input)
                |> Set.unionMany

            let parsed =
                parsersAndInputs
                |> Seq.map (fun (TextParserAndSampleInput (_, input)) -> parser input)
                |> Set.unionMany

            expectedOutputs |> Set.forall (fun result -> parsed |> Set.contains result)

        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``choice of one is equivalent to the single parser`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParser = [ parser ] |> TextParser.choice |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``empty choice is equivalent to fail`` (parserType : TextParserType) =
        let prop (input : string) =
            let actualParser = [ ] |> TextParser.choice |> makeParser parserType
            let expectedParser = TextParser.fail |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a left unit of binary choice`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParser = TextParser.fail .+. parser |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a right unit of binary choice`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParser = parser .+. TextParser.fail |> makeParser parserType
            let expectedParser = parser |> makeParser parserType
            actualParser input = expectedParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``binary choice is commutative`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, _)) =
            let p1 = parser1 .+. parser2 |> makeParser parserType
            let p2 = parser2 .+. parser1 |> makeParser parserType
            p1 input1 = p2 input1
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``binary choice is associative`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, _)) (TextParserAndSampleInput (parser3, _)) =
            let p1 = (parser1 .+. parser2) .+. parser3 |> makeParser parserType
            let p2 = parser1 .+. (parser2 .+. parser3) |> makeParser parserType
            p1 input1 = p2 input1
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence parses the concatenation of strings parsed by its constituent parsers`` (parserType : TextParserType) =

        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, input2)) =
            match makeParser parserType parser1 (input1 + input2) |> Set.toList with
            | [] ->
                let actualParsed = makeParser parserType (TextParser.sequence parser1 parser2) (input1 + input2)
                let expectedParsed = Set.empty
                actualParsed = expectedParsed
            | (parsed1, _)::_ ->
                let parsed2 = makeParser parserType parser2 input2
                let actualParsed = makeParser parserType (TextParser.sequence parser1 parser2) (parsed1 + input2)
                let expectedParsed = parsed2 |> Set.map (fun (parsed, rest) -> parsed1 + parsed, rest)
                expectedParsed |> Set.forall (fun expected -> actualParsed |> Set.contains expected)

        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence distributes over binary choice to the left`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, input2)) (TextParserAndSampleInput (parser3, input3)) =
            let p1 = parser1 .*. (parser2 .+. parser3)
            let p2 = parser1 .*. parser2 .+. parser1 .*. parser3
            let input = input1 + input2 + input3
            makeParser parserType p1 input = makeParser parserType p2 input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence distributes over binary choice to the right`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, input2)) (TextParserAndSampleInput (parser3, input3)) =
            let p1 = (parser1 .+. parser2) .*. parser3
            let p2 = parser1 .*. parser3 .+. parser2 .*. parser3
            let input = input1 + input2 + input3
            makeParser parserType p1 input = makeParser parserType p2 input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a left zero of sequence`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParsed = makeParser parserType (TextParser.sequence TextParser.fail parser) input
            let expectedParsed = Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``fail is a right zero of sequence`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParsed = makeParser parserType (TextParser.sequence parser TextParser.fail) input
            let expectedParsed = Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success is a left unit of sequence`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParsed = makeParser parserType (TextParser.sequence TextParser.success parser) input
            let expectedParsed = makeParser parserType parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``success is a right unit of sequence`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParsed = makeParser parserType (TextParser.sequence parser TextParser.success) input
            let expectedParsed = makeParser parserType parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``sequence is associative`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, input2)) (TextParserAndSampleInput (parser3, input3)) =
            let p1 = (parser1 .*. parser2) .*. parser3 |> makeParser parserType
            let p2 = parser1 .*. (parser2 .*. parser3) |> makeParser parserType
            let input = input1 + input2 + input3
            p1 input = p2 input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``zeroOrMore returns all n+1 possible parses when parsing a string repeated n times`` (parserType : TextParserType) =
        let prop (NonEmptyString s) (SmallInt n) =
            let parser = TextParser.string s |> TextParser.zeroOrMore |> makeParser parserType
            let input = String.replicate n s
            let expectedParsed = [0..n] |> List.map (fun i -> String.replicate i s, String.replicate (n - i) s) |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``zeroOrMore fail is equivalent to success`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let expectedParser = TextParser.success |> makeParser parserType
            let actualParser = TextParser.zeroOrMore TextParser.fail |> makeParser parserType
            expectedParser input = actualParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``oneOrMore returns all n possible parses when parsing a string repeated n times`` (parserType : TextParserType) =
        let prop (NonEmptyString s) (SmallInt n) =
            let parser = TextParser.string s |> TextParser.oneOrMore |> makeParser parserType
            let input = String.replicate n s
            let expectedParsed = [1..n] |> List.map (fun i -> String.replicate i s, String.replicate (n - i) s) |> Set.ofSeq
            let actualParsed = parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``oneOrMore fail is equivalent to fail`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let expectedParser = TextParser.fail |> makeParser parserType
            let actualParser = TextParser.oneOrMore TextParser.fail |> makeParser parserType
            expectedParser input = actualParser input
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``bind behaves in the same way as sequence, for a fixed second parser`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser1, input1)) (TextParserAndSampleInput (parser2, input2)) =
            let input = input1 + input2
            let actualParsed = makeParser parserType (TextParser.bind (fun _ -> parser2) parser1) input
            let expectedParsed = makeParser parserType (parser1 .*. parser2) input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``filtering a parser is equivalent to filtering its parses`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) (TextFilterFunction f) =
            let actualParsed = makeParser parserType (TextParser.filter f parser) input
            let expectedParsed = makeParser parserType parser input |> Set.filter (fst >> f)
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``a delayed parser behaves exactly the same as the parser itself`` (parserType : TextParserType) =
        let prop (TextParserAndSampleInput (parser, input)) =
            let actualParsed = makeParser parserType (TextParser.delay (fun () -> parser)) input
            let expectedParsed = makeParser parserType parser input
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``character parses the first character of the input when it matches the supplied character`` (parserType : TextParserType) =
        let prop (c : char) (NonNull input) =
            let actualParsed = makeParser parserType (TextParser.character c) (c.ToString () + input)
            let expectedParsed = Set.singleton (c.ToString (), input)
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``character parses the first character of the input iff it matches the supplied character`` (parserType : TextParserType) =
        let prop (c : char) (NonNull input) =
            let actualParsed = makeParser parserType (TextParser.character c) input
            let expectedParsed = if input.Length > 0 && input.[0] = c then Set.singleton (c.ToString () , input.[1..]) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``string parses the initial segment of the input when it matches the supplied string`` (parserType : TextParserType) =
        let prop (NonNull s1) (NonNull s2) =
            let actualParsed = makeParser parserType (TextParser.string s1) (s1 + s2)
            let expectedParsed = Set.singleton (s1, s2)
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``string parses the initial segment of the input iff it matches the supplied string`` (parserType : TextParserType) =
        let prop (NonNull s1) (NonNull s2) =
            let actualParsed = makeParser parserType (TextParser.string s1) s2
            let expectedParsed = if s2.StartsWith s1 then Set.singleton (s1 , s2.Substring s1.Length) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``letter parses the first character of the input iff it is a letter`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let actualParsed = makeParser parserType TextParser.letter input
            let expectedParsed = if input.Length > 0 && System.Char.IsLetter input.[0] then Set.singleton (input.[0..0] , input.[1..]) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``digit parses the first character of the input iff it is a digit`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let actualParsed = makeParser parserType TextParser.digit input
            let expectedParsed = if input.Length > 0 && System.Char.IsDigit input.[0] then Set.singleton (input.[0..0] , input.[1..]) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``letterOrDigit parses the first character of the input iff it is a letter or a digit`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let actualParsed = makeParser parserType TextParser.letterOrDigit input
            let expectedParsed = if input.Length > 0 && System.Char.IsLetterOrDigit input.[0] then Set.singleton (input.[0..0] , input.[1..]) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Theory>]
    [<MemberData(allParserTypes)>]
    let ``newLine parses the first characters of the input iff they represent a new line`` (parserType : TextParserType) =
        let prop (NonNull input) =
            let newLine = System.Environment.NewLine
            let actualParsed = makeParser parserType TextParser.newLine input
            let expectedParsed = if input.StartsWith newLine then Set.singleton (newLine , input.[newLine.Length..]) else Set.empty
            actualParsed = expectedParsed
        check prop

    [<Fact>]
    let ``ReferenceTextParser and OptimisedTextParser return the same parses`` () =
        let prop (TextParserAndSampleInput (parser, input)) =
            makeParser TextParserType.ReferenceTextParser parser input = makeParser TextParserType.OptimisedTextParser parser input
        check prop

    [<Fact>]
    let ``All parses of OptimisedTextParser are initial segments of the input`` () =
        let prop (TextParserAndSampleInput (parser, input)) =
            let parses = ResizeArray ()
            OptimisedTextParser.make parser parses.Add input 0
            parses |> Seq.forall (fun segment -> segment.Offset = 0)
        check prop

    [<Fact>]
    let ``OptimisedTextParser can begin parsing from the middle of a StringSegment correctly`` () =
        let prop (TextParserAndSampleInput (parser, input)) (NonNull s) =

            let parse input offset =
                let parses = ResizeArray ()
                OptimisedTextParser.make parser parses.Add input offset
                parses
                |> Seq.map (fun segment -> segment |> StringSegment.current, segment |> StringSegment.remaining)
                |> Set.ofSeq

            let expected = parse input 0
            let actual = parse (s + input) s.Length
            expected = actual
        check prop
