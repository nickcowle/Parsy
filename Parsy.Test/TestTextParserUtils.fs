namespace Parsy.Test

open FsCheck
open Parsy

[<NoEquality>]
[<NoComparison>]
type ParserAndSampleInput = ParserAndSampleInput of TextParser * string

type ParserType =
| ReferenceTextParser
| OptimisedTextParser

[<RequireQualifiedAccess>]
module TestTextParserUtils =

    let makeParser =
        function
        | ReferenceTextParser ->
            fun parser input ->
                ReferenceTextParser.make parser input
                |> Set.ofList
        | OptimisedTextParser ->
            fun parser input ->
                let parses = ResizeArray ()
                OptimisedTextParser.make parser parses.Add (StringSegment.ofString input)
                parses
                |> Seq.map (fun segment -> segment |> StringSegment.consumed, segment |> StringSegment.remaining)
                |> Set.ofSeq

    type Marker = Marker

    let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<Marker>.DeclaringType ] ; MaxTest = 10_000 ; EndSize = 4 }

    let check prop = Check.One(config, prop)

    let parsersAndInputs : Arbitrary<ParserAndSampleInput> =

        let make p i = ParserAndSampleInput (p, i)

        let parser (ParserAndSampleInput (parser, _)) = parser

        let input (ParserAndSampleInput (_ , input)) = input

        let success =
            let makeSuccess (NonNull s) = make TextParser.success s
            Arb.generate |> Gen.map makeSuccess

        let fail =
            let makeFail (NonNull s) = make TextParser.fail s
            Arb.generate |> Gen.map makeFail

        let choice (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeChoice (ps : ParserAndSampleInput list) =
                match ps with
                | [] ->
                    Arb.generate |> Gen.map (fun (NonNull s) -> make ([] |> TextParser.choice) s)
                | p::_ ->
                    make (ps |> List.map parser |> TextParser.choice) (p |> input)
                    |> Gen.constant

            parsersAndInputs |> Gen.listOf >>= makeChoice

        let sequence (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeSequence (p1 : ParserAndSampleInput) (p2 : ParserAndSampleInput) =
                make (TextParser.sequence (parser p1) (parser p2)) (input p1 + input p2)

            parsersAndInputs |> Gen.map makeSequence <*> parsersAndInputs

        let zeroOrMore (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeZeroOrMore (ParserAndSampleInput (parser, input)) (c : char) (n : int) =
                let parser = TextParser.sequence (TextParser.character c) parser |> TextParser.zeroOrMore
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeZeroOrMore <*> Arb.generate <*> Gen.elements [0..3]

        let oneOrMore (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeOneOrMore (ParserAndSampleInput (parser, input)) (c : char) (n : int) =
                let parser = TextParser.sequence (TextParser.character c) parser |> TextParser.oneOrMore
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeOneOrMore <*> Arb.generate <*> Gen.elements [0..3]

        let bind (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeBind (p1 : ParserAndSampleInput) (p2 : ParserAndSampleInput) =
                make (TextParser.bind (fun _ -> parser p2) (parser p1)) (input p1 + input p2)

            parsersAndInputs |> Gen.map makeBind <*> parsersAndInputs

        let filter (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeFilter (ParserAndSampleInput (parser, input)) filter =
                make (TextParser.filter filter parser) input

            let filters =
                [
                    fun _ -> true
                    fun _ -> false
                    fun (s : string) -> s.Length % 2 = 0
                    fun (s : string) -> s.Length % 2 = 1
                ]
                |> Gen.elements

            parsersAndInputs |> Gen.map makeFilter <*> filters

        let delay (parsersAndInputs : ParserAndSampleInput Gen) =

            let makeDelay (ParserAndSampleInput (parser, input)) =
                make (TextParser.delay (fun () -> parser)) input

            parsersAndInputs |> Gen.map makeDelay

        let character =
            [
                Arb.generate |> Gen.map (fun c -> make (TextParser.character c) (c.ToString()))
                Arb.generate |> Gen.map (fun c (NonNull s) -> make (TextParser.character c) (c.ToString() + s)) <*> Arb.generate
                Arb.generate |> Gen.map (fun c (NonNull s) -> make (TextParser.character c) s) <*> Arb.generate
            ]
            |> Gen.oneof

        let string =
            [
                Arb.generate |> Gen.map (fun (NonNull s1) -> make (TextParser.string s1) s1)
                Arb.generate |> Gen.map (fun (NonNull s1) (NonNull s2) -> make (TextParser.string s1) (s1 + s2)) <*> Arb.generate
                Arb.generate |> Gen.map (fun (NonNull s1) (NonNull s2) -> make (TextParser.string s1) s2) <*> Arb.generate
            ]
            |> Gen.oneof

        let letters = ['a'..'z'] @ ['A'..'Z'] |> Gen.elements

        let digits = ['0'..'9'] |> Gen.elements

        let lettersAndDigits = Gen.oneof [ letters ; digits ]

        let letter =
            [
                letters |> Gen.map (fun c -> make TextParser.letter (c.ToString()))
                letters |> Gen.map (fun c (NonNull s) -> make TextParser.letter (c.ToString() + s)) <*> Arb.generate
                Arb.generate |> Gen.map (fun (NonNull s) -> make TextParser.letter s)
            ]
            |> Gen.oneof

        let digit =
            [
                digits |> Gen.map (fun c -> make TextParser.digit (c.ToString()))
                digits |> Gen.map (fun c (NonNull s) -> make TextParser.digit (c.ToString() + s)) <*> Arb.generate
                Arb.generate |> Gen.map (fun (NonNull s) -> make TextParser.digit s)
            ]
            |> Gen.oneof

        let letterOrDigit =
            [
                lettersAndDigits |> Gen.map (fun c -> make TextParser.letterOrDigit (c.ToString()))
                lettersAndDigits |> Gen.map (fun c (NonNull s) -> make TextParser.letterOrDigit (c.ToString() + s)) <*> Arb.generate
                Arb.generate |> Gen.map (fun (NonNull s) -> make TextParser.letterOrDigit s)
            ]
            |> Gen.oneof

        let rec parsersAndInputsSized n =

            match n with
            | 0 ->
                [
                    success
                    fail
                    character
                    string
                    letter
                    digit
                    letterOrDigit
                ]
                |> Gen.oneof
            | _ ->

                let parsersAndInputs = parsersAndInputsSized (n - 1)

                [
                    success
                    fail
                    choice     parsersAndInputs
                    sequence   parsersAndInputs
                    zeroOrMore parsersAndInputs
                    oneOrMore  parsersAndInputs
                    bind       parsersAndInputs
                    filter     parsersAndInputs
                    delay      parsersAndInputs
                    character
                    string
                    letter
                    digit
                    letterOrDigit
                ]
                |> Gen.oneof

        parsersAndInputsSized
        |> Gen.sized
        |> Arb.fromGen
