namespace Parsy.Test

open FsCheck
open Parsy

[<NoEquality>]
[<NoComparison>]
type 'a ParserAndSampleInput = ParserAndSampleInput of 'a Parser * string

[<NoEquality>]
[<NoComparison>]
type 'a CombinerFunction = CombinerFunction of ('a -> 'a -> 'a)

[<NoEquality>]
[<NoComparison>]
type 'a AssociativeCombinerFunction = AssociativeCombinerFunction of ('a -> 'a -> 'a)

[<NoEquality>]
[<NoComparison>]
type 'a MappingFunction = MappingFunction of ('a -> 'a)

[<RequireQualifiedAccess>]
module ParserGenerator =

    type Marker = Marker

    let intCombinerFunctions : int CombinerFunction Arbitrary =
        [
            (+)
            (-)
            (*)
        ]
        |> List.map CombinerFunction
        |> Gen.elements
        |> Arb.fromGen

    let intAssociativeCombinerFunctions : int AssociativeCombinerFunction Arbitrary =
        [
            (+)
            (*)
        ]
        |> List.map AssociativeCombinerFunction
        |> Gen.elements
        |> Arb.fromGen

    let intMappingFunctions : int MappingFunction Arbitrary =
        [
            fun i -> i + 10
            fun i -> i - 5
            fun i -> 0
            fun i -> -3
            fun i -> i % 13
        ]
        |> List.map MappingFunction
        |> Gen.elements
        |> Arb.fromGen

    let parsersAndInputs<'a> : 'a ParserAndSampleInput Arbitrary =

        let make p i = ParserAndSampleInput (p, i)

        let parser (ParserAndSampleInput (parser, _)) = parser

        let input (ParserAndSampleInput (_ , input)) = input

        let textParser =
            let makeTextParser (TextParserAndSampleInput (parser, input)) =
                make (Parser.ofTextParser parser) input
            makeTextParser <!> Arb.generate

        let success =
            let makeSuccess (a : 'a) (NonNull s) = make (Parser.success a) s
            makeSuccess <!> Arb.generate <*> Arb.generate

        let fail =
            let makeFail (NonNull s) = make Parser.fail s
            makeFail <!> Arb.generate

        let choice (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeChoice (ps : 'a ParserAndSampleInput list) =
                match ps with
                | [] ->
                    let makeChoiceEmpty (NonNull s) = make (Parser.choice []) s
                    makeChoiceEmpty <!> Arb.generate
                | p::_ ->
                    make (ps |> List.map parser |> Parser.choice) (p |> input)
                    |> Gen.constant

            parsersAndInputs |> Gen.listOf >>= makeChoice

        let sequence (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeSequence (CombinerFunction f) (p1 : 'a ParserAndSampleInput) (p2 : 'a ParserAndSampleInput) =
                make (Parser.sequence f (parser p1) (parser p2)) (input p1 + input p2)

            makeSequence <!> Arb.generate <*> parsersAndInputs <*> parsersAndInputs

        let map (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeMap (MappingFunction f) (ParserAndSampleInput (parser, input)) =
                make (Parser.map f parser) input

            makeMap <!> Arb.generate <*> parsersAndInputs

        let bind (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeBind (p1 : 'a ParserAndSampleInput) (p2 : 'a ParserAndSampleInput) =
                make (Parser.bind (fun _ -> parser p2) (parser p1)) (input p1 + input p2)

            parsersAndInputs |> Gen.map makeBind <*> parsersAndInputs

        let prefixWithDummyChar (c : char) (parser : 'a Parser) : 'a Parser =
            let dummyChar = TextParser.character c |> Parser.ofTextParser
            Parser.sequence (fun _ -> id) dummyChar parser

        let zeroOrMore (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeZeroOrMore
                (ParserAndSampleInput (parser, input)) (c : char) (n : int)
                (CombinerFunction f) (s : 'a) =
                // N.B. we need to introduce a dummy character here because any parser
                // that parses the empty input will cause an infinite loop when nested
                // inside zeroOrMore
                let parser = prefixWithDummyChar c parser
                let parser = Parser.zeroOrMore s f parser
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeZeroOrMore <*> Arb.generate <*> Gen.elements [0..3] <*> Arb.generate <*> Arb.generate

        let oneOrMore (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeOneOrMore
                (ParserAndSampleInput (parser, input)) (c : char) (n : int)
                (CombinerFunction f) (MappingFunction s) =
                // N.B. we need to introduce a dummy character here because any parser
                // that parses the empty input will cause an infinite loop when nested
                // inside oneOrMore
                let parser = prefixWithDummyChar c parser
                let parser = Parser.oneOrMore s f parser
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeOneOrMore <*> Arb.generate <*> Gen.elements [0..3] <*> Arb.generate <*> Arb.generate

        let interleave (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeInterleave
                (ParserAndSampleInput (parser1, input1))
                (ParserAndSampleInput (parser2, input2))
                (CombinerFunction f) (CombinerFunction g)
                (MappingFunction s)
                (c : char) (n : int) =
                let parser1 = prefixWithDummyChar c parser1
                let parser = Parser.interleave s (fun s b a -> g (f s b) a) parser1 parser2
                let input =
                    let input1 = sprintf "%c%s" c input1
                    sprintf "%s%s" input1 (sprintf "%s%s" input2 input2 |> String.replicate n)
                make parser input

            parsersAndInputs
            |> Gen.map makeInterleave
            <*> parsersAndInputs
            <*> Arb.generate <*> Arb.generate <*> Arb.generate <*> Arb.generate
            <*> Gen.elements [0..3]

        let interleave1 (parsersAndInputs : 'a ParserAndSampleInput Gen) =

            let makeInterleave
                (ParserAndSampleInput (parser1, input1))
                (ParserAndSampleInput (parser2, input2))
                (CombinerFunction f) (CombinerFunction g)
                (c : char) (n : int) =
                let parser1 = prefixWithDummyChar c parser1
                let parser = Parser.interleave1 (fun a1 b a2 -> g (f a1 b) a2) (fun s b a -> g (f s b) a) parser1 parser2
                let input =
                    let input1 = sprintf "%c%s" c input1
                    sprintf "%s%s" input1 (sprintf "%s%s" input2 input2 |> String.replicate n)
                make parser input

            parsersAndInputs
            |> Gen.map makeInterleave
            <*> parsersAndInputs
            <*> Arb.generate <*> Arb.generate <*> Arb.generate
            <*> Gen.elements [0..3]

        let rec parsersAndInputsSized n =
            [
                if typeof<'a> = typeof<string> then
                    yield textParser |> unbox

                yield success
                yield fail

                if n > 0 then
                    let parsersAndInputs = parsersAndInputsSized (n - 1)
                    yield choice parsersAndInputs
                    yield sequence parsersAndInputs
                    yield map parsersAndInputs
                    yield bind parsersAndInputs
                    yield zeroOrMore parsersAndInputs
                    yield oneOrMore parsersAndInputs
                    yield interleave parsersAndInputs
                    yield interleave1 parsersAndInputs
            ]
            |> Gen.oneof

        parsersAndInputsSized
        |> Gen.sized
        |> Arb.fromGen
