namespace Parsy

[<RequireQualifiedAccess>]
module OptimisedTextParser =

    type Parser = (StringSegment -> unit) -> string -> int -> unit

    let inline parseFromSegment p sink segment = p sink segment.Value (segment.Offset + segment.Length)

    let union segment1 segment2 =
        segment1 |> StringSegment.extend segment2.Length

    let success : Parser =
        fun sink input offset -> sink (StringSegment.make input offset 0)

    let fail : Parser =
        fun _ _ _ -> ()

    let choice (parsers : Parser list) : Parser =
        fun sink ->
            let parsers = parsers |> List.map ((|>) sink)
            fun input offset ->
                for parser in parsers do
                    parser input offset

    let sequence (p1 : Parser) (p2 : Parser) : Parser =
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink p1Parse =
                parseFromSegment p2 p2Parses.Add p1Parse
                for p2Parse in p2Parses do
                    union p1Parse p2Parse |> sink
                p2Parses.Clear ()
            fun input offset ->
                p1 p1Sink input offset

    let delay (f : unit -> Parser) : Parser =
        let parser = lazy f ()
        fun sink -> parser.Value sink

    let rec zeroOrMore (p : Parser) : Parser =
        fun sink ->
            let oneOrMore = oneOrMore p sink
            fun input offset ->
                StringSegment.make input offset 0 |> sink
                oneOrMore input offset

    and oneOrMore (p : Parser) : Parser =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let nextParses = ResizeArray ()
            fun input offset ->
                p parses.Enqueue input offset
                while parses.Count > 0 do
                    let parse = parses.Dequeue ()
                    sink parse
                    parseFromSegment p nextParses.Add parse
                    for parse2 in nextParses do
                        union parse parse2 |> parses.Enqueue
                    nextParses.Clear ()

    let bind (f : string -> Parser) (p1 : Parser) : Parser =
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink p1Parse =
                let p2 = f (p1Parse |> StringSegment.current)
                parseFromSegment p2 p2Parses.Add p1Parse
                for p2Parse in p2Parses do
                    union p1Parse p2Parse |> sink
                p2Parses.Clear ()
            p1 p1Sink

    let filter (f : string -> bool) (p : Parser) : Parser =
        fun sink ->
            let sink input = if f (input |> StringSegment.current) then sink input
            p sink

    let character (pred : char -> bool) : Parser =
        fun sink input offset ->
            if input.Length > offset then
                let c = input.[offset]
                if pred c then sink (StringSegment.make input offset 1)

    let string (s : string) : Parser =
        fun sink input offset ->
            if System.String.CompareOrdinal(s, 0, input, offset, s.Length) = 0 then
                sink (StringSegment.make input offset s.Length)

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
