namespace Parsy.Test


open FsCheck
open Parsy


[<NoEquality>]
[<NoComparison>]
type TextParserAndSampleInput = TextParserAndSampleInput of TextParser * string


[<RequireQualifiedAccess>]
module TextParserGenerator =

    type Marker = Marker

    let parsersAndInputs : Arbitrary<TextParserAndSampleInput> =

        let make p i = TextParserAndSampleInput (p, i)

        let parser (TextParserAndSampleInput (parser, _)) = parser

        let input (TextParserAndSampleInput (_ , input)) = input

        let success =
            let makeSuccess (NonNull s) = make TextParser.success s
            Arb.generate |> Gen.map makeSuccess

        let fail =
            let makeFail (NonNull s) = make TextParser.fail s
            Arb.generate |> Gen.map makeFail

        let choice (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeChoice (ps : TextParserAndSampleInput list) =
                match ps with
                | [] ->
                    Arb.generate |> Gen.map (fun (NonNull s) -> make ([] |> TextParser.choice) s)
                | p::_ ->
                    make (ps |> List.map parser |> TextParser.choice) (p |> input)
                    |> Gen.constant

            parsersAndInputs |> Gen.listOf >>= makeChoice

        let sequence (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeSequence (p1 : TextParserAndSampleInput) (p2 : TextParserAndSampleInput) =
                make (TextParser.sequence (parser p1) (parser p2)) (input p1 + input p2)

            parsersAndInputs |> Gen.map makeSequence <*> parsersAndInputs

        let zeroOrMore (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeZeroOrMore (TextParserAndSampleInput (parser, input)) (c : char) (n : int) =
                let parser = TextParser.sequence (TextParser.character c) parser |> TextParser.zeroOrMore
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeZeroOrMore <*> Arb.generate <*> Gen.elements [0..3]

        let oneOrMore (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeOneOrMore (TextParserAndSampleInput (parser, input)) (c : char) (n : int) =
                let parser = TextParser.sequence (TextParser.character c) parser |> TextParser.oneOrMore
                let input = sprintf "%c%s" c input |> String.replicate n
                make parser input

            parsersAndInputs |> Gen.map makeOneOrMore <*> Arb.generate <*> Gen.elements [0..3]

        let bind (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeBind (p1 : TextParserAndSampleInput) (p2 : TextParserAndSampleInput) =
                make (TextParser.bind (fun _ -> parser p2) (parser p1)) (input p1 + input p2)

            parsersAndInputs |> Gen.map makeBind <*> parsersAndInputs

        let filter (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeFilter (TextParserAndSampleInput (parser, input)) filter =
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

        let delay (parsersAndInputs : TextParserAndSampleInput Gen) =

            let makeDelay (TextParserAndSampleInput (parser, input)) =
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

        let newLine =
            [
                make TextParser.newLine System.Environment.NewLine |> Gen.constant
                Arb.generate |> Gen.map (fun (NonNull s) -> make TextParser.newLine (System.Environment.NewLine + s))
                Arb.generate |> Gen.map (fun (NonNull s) -> make TextParser.newLine s)
            ]
            |> Gen.oneof

        let rec parsersAndInputsSized n =
            [
                yield success
                yield fail
                yield character
                yield string
                yield letter
                yield digit
                yield letterOrDigit
                yield newLine

                if n > 0 then
                    let parsersAndInputs = parsersAndInputsSized (n - 1)
                    yield choice     parsersAndInputs
                    yield sequence   parsersAndInputs
                    yield zeroOrMore parsersAndInputs
                    yield oneOrMore  parsersAndInputs
                    yield bind       parsersAndInputs
                    yield filter     parsersAndInputs
                    yield delay      parsersAndInputs
            ]
            |> Gen.oneof

        parsersAndInputsSized
        |> Gen.sized
        |> Arb.fromGen

