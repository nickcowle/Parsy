namespace Parsy

[<Struct>]
type StringSegment =
    {
        Value : string
        Offset : int
        Length : int
    }


[<RequireQualifiedAccess>]
module StringSegment =

    let ofString s =
        {
            Value = s
            Offset = 0
            Length = 0
        }

    let advance amount segment =
        {
            Value = segment.Value
            Offset = segment.Offset + segment.Length
            Length = amount
        }

    let current segment =
        segment.Value.Substring(segment.Offset, segment.Length)

    let consumed segment =
        segment.Value.Substring(0, segment.Offset + segment.Length)

    let remaining segment =
        segment.Value.Substring(segment.Offset + segment.Length)


[<RequireQualifiedAccess>]
module OptimisedTextParser =

    type Parser = (StringSegment -> unit) -> StringSegment -> unit

    let success : Parser =
        fun sink input -> sink (input |> StringSegment.advance 0)

    let fail : Parser =
        fun _ _ -> ()

    let choice (parsers : Parser list) : Parser =
        fun sink input ->
            for parser in parsers do
                parser sink input

    let sequence (p1 : Parser) (p2 : Parser) : Parser =
        fun sink -> p1 (p2 sink)

    let delay (f : unit -> Parser) : Parser =
        let parser = lazy f ()
        fun input -> parser.Value input

    let rec zeroOrMore (p : Parser) : Parser =
        choice [ success ; delay (fun () -> oneOrMore p) ]

    and oneOrMore (p : Parser) : Parser =
        sequence p (zeroOrMore p)

    let bind (f : string -> Parser) (p : Parser) : Parser =
        fun sink ->
            let sink input = f (input |> StringSegment.current) sink input
            p sink

    let filter (f : string -> bool) (p : Parser) : Parser =
        fun sink ->
            let sink input = if f (input |> StringSegment.current) then sink input
            p sink

    let character (pred : char -> bool) : Parser =
        fun sink input ->
            if input.Value.Length > input.Offset + input.Length then
                let c = input.Value.[input.Offset + input.Length]
                if pred c then sink (input |> StringSegment.advance 1)

    let string (s : string) : Parser =
        fun sink input ->
            if System.String.CompareOrdinal(s, 0, input.Value, input.Offset + input.Length, s.Length) = 0 then
                sink (input |> StringSegment.advance s.Length)

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
