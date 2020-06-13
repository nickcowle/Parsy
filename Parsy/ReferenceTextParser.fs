namespace Parsy

[<RequireQualifiedAccess>]
module ReferenceTextParser =

    type Parser = string -> (string * string) list


    type TextParserBuilder () =

        member __.Yield (s : string) : Parser =
            fun input -> [ s, input ]

        member __.YieldFrom (p : Parser) : Parser =
            p

        member __.Bind (p : Parser, f : string -> Parser) : Parser =
            fun input ->
                p input |> List.collect (fun (parsed, rest) -> f parsed rest)

        member __.Zero () : Parser =
            fun _ -> []

        member __.For (xs : 'a list, f : 'a -> Parser) =
            let parsers = xs |> List.map f
            fun input -> parsers |> List.collect ((|>) input)


    let textParser = TextParserBuilder ()


    let success =
        textParser {
            yield ""
        }

    let fail =
        textParser {
            ()
        }

    let choice (parsers : Parser list) : Parser =
        textParser {
            for parser in parsers do
                yield! parser
        }

    let sequence (p1 : Parser) (p2 : Parser) : Parser =
        textParser {
            let! s1 = p1
            let! s2 = p2
            yield s1 + s2
        }

    let delay (f : unit -> Parser) : Parser =
        let parser = lazy f ()
        fun input -> parser.Value input

    let rec zeroOrMore (p : Parser) : Parser =
        choice [ success ; delay (fun () -> oneOrMore p) ]

    and oneOrMore (p : Parser) : Parser =
        sequence p (zeroOrMore p)

    let bind (f : string -> Parser) (p : Parser) : Parser =
        textParser {
            let! s1 = p
            let! s2 = f s1
            yield s1 + s2
        }

    let filter (f : string -> bool) (p : Parser) : Parser =
        textParser {
            let! s = p
            if f s then
                yield s
        }

    let character (pred : char -> bool) : Parser =
        fun (input : string) ->
            if input.Length > 0 && pred input.[0] then
                [ input.[0..0], input.[1..] ]
            else
                []

    let string (s : string) : Parser =
        fun (input : string) ->
            if input.StartsWith s then
                [ input.[0..s.Length - 1], input.[s.Length..] ]
            else
                []

    let rec make (parser : TextParser) : Parser =
        match parser with
        | Success -> success
        | Fail -> fail
        | Choice parsers -> parsers |> List.map make |> choice
        | Sequence (p1, p2) -> sequence (make p1) (make p2)
        | ZeroOrMore parser -> zeroOrMore (make parser)
        | OneOrMore parser -> oneOrMore (make parser)
        | Bind (f, p) -> bind (f >> make) (make p)
        | Filter (f, p) -> filter f (make p)
        | Delay f -> delay (f >> make)
        | Character c -> character ((=) c)
        | String s -> string s
        | Letter -> character System.Char.IsLetter
        | Digit -> character System.Char.IsDigit
        | LetterOrDigit -> character System.Char.IsLetterOrDigit
