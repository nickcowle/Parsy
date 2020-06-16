namespace Parsy

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
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink parsed =
                p2 p2Parses.Add parsed
                for parsed2 in p2Parses do
                    StringSegment.extend parsed parsed2.Length |> sink
                p2Parses.Clear ()
            p1 p1Sink

    let delay (f : unit -> Parser) : Parser =
        let parser = lazy f ()
        fun input -> parser.Value input

    let rec zeroOrMore (p : Parser) : Parser =
        fun sink ->
            let oneOrMore = oneOrMore p sink
            fun input ->
                let segment = input |> StringSegment.advance 0
                sink segment
                oneOrMore segment

    and oneOrMore (p : Parser) : Parser =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let nextParses = ResizeArray ()
            fun input ->
                p parses.Enqueue input
                while parses.Count > 0 do
                    let parse = parses.Dequeue ()
                    sink parse
                    p nextParses.Add parse
                    for parse2 in nextParses do
                        StringSegment.extend parse parse2.Length |> parses.Enqueue
                    nextParses.Clear ()

    let bind (f : string -> Parser) (p1 : Parser) : Parser =
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink parsed =
                f (parsed |> StringSegment.current) p2Parses.Add parsed
                for parsed2 in p2Parses do
                    StringSegment.extend parsed parsed2.Length |> sink
                p2Parses.Clear ()
            p1 p1Sink

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
