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

        let rec parsersAndInputsSized n =

            let ifString =
                if typeof<'a> = typeof<string> then
                    [
                        textParser |> unbox
                    ]
                else
                    []

            let small =
                [
                    success
                    fail
                ]

            let large =
                if n > 0  then

                    let parsersAndInputs = parsersAndInputsSized (n - 1)

                    [
                        choice parsersAndInputs
                        sequence parsersAndInputs
                        map parsersAndInputs
                    ]
                else
                    []

            ifString @ small @ large
            |> Gen.oneof

        parsersAndInputsSized
        |> Gen.sized
        |> Arb.fromGen
