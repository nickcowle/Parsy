namespace Parsy

open TypeEquality

[<RequireQualifiedAccess>]
module OptimisedParser =

    type 'a ParseFun = ('a -> StringSegment -> unit) -> string -> int -> unit

    let inline parseFromSegment p sink segment = p sink segment.Value (segment.Offset + segment.Length)

    let union segment1 segment2 =
        segment1 |> StringSegment.extend segment2.Length

    let textParser (textParser : TextParser) : string ParseFun =
        let textParser = textParser |> TextParserReducer.reduce
        fun sink ->
            OptimisedTextParser.make textParser (fun segment -> sink (segment |> StringSegment.current) segment)

    let success (a : 'a) : 'a ParseFun =
        fun sink input offset ->
            sink a (StringSegment.make input offset 0)

    let fail<'a> : 'a ParseFun =
        fun _ _ _ -> ()

    let choice (parsers : 'a ParseFun list) : 'a ParseFun =
        fun sink ->
            let parsers = parsers |> List.map ((|>) sink)
            fun input offset ->
                for parser in parsers do
                    parser input offset

    let sequence (f : 'a -> 'b -> 'c) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 'c ParseFun =
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink a p1Parse =
                parseFromSegment p2 (fun b p2Parse -> struct (b, p2Parse) |> p2Parses.Add) p1Parse
                for (struct (b, p2Parse)) in p2Parses do
                    sink (f a b) (union p1Parse p2Parse)
                p2Parses.Clear ()
            fun input offset ->
                p1 p1Sink input offset

    let map (f : 'a -> 'b) (p : 'a ParseFun) : 'b ParseFun =
        fun sink ->
            let sink a segment = sink (f a) segment
            p sink

    let bind (f : 'a -> 'b ParseFun) (p : 'a ParseFun) : 'b ParseFun =
        fun sink ->
            let p2Parses = ResizeArray ()
            let p1Sink a p1Parse =
                let p2 = f a
                parseFromSegment p2 (fun b p2Parse -> struct (b, p2Parse) |> p2Parses.Add) p1Parse
                for (struct (b, p2Parse)) in p2Parses do
                    sink b (union p1Parse p2Parse)
                p2Parses.Clear ()
            fun input offset ->
                p p1Sink input offset

    let rec zeroOrMore (s : 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let nextParses = ResizeArray ()
            let addToNextParses a parsed = struct (a, parsed) |> nextParses.Add
            fun input offset ->
                parses.Enqueue struct (s, StringSegment.make input offset 0)
                while parses.Count > 0 do
                    let struct (s, parse) = parses.Dequeue ()
                    sink s parse
                    parseFromSegment p addToNextParses parse
                    for (struct (a, parse2)) in nextParses do
                        parses.Enqueue (f s a, union parse parse2)
                    nextParses.Clear ()

    and oneOrMore (s : 'a -> 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let addToParses a parse = struct (s a, parse) |> parses.Enqueue
            let nextParses = ResizeArray ()
            let addToNextParses a parsed = struct (a, parsed) |> nextParses.Add
            fun input offset ->
                p addToParses input offset
                while parses.Count > 0 do
                    let struct (s, parse) = parses.Dequeue ()
                    sink s parse
                    parseFromSegment p addToNextParses parse
                    for (struct (a, parse2)) in nextParses do
                        parses.Enqueue (f s a, union parse parse2)
                    nextParses.Clear ()

    let interleave (s : 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let addToParses a parse = struct (s a, parse) |> parses.Enqueue
            let nextBParses = ResizeArray ()
            let addToNextBParses b parsed = struct (b, parsed) |> nextBParses.Add
            let nextAParses = ResizeArray ()
            let addToNextAParses a parsed = struct (a, parsed) |> nextAParses.Add
            fun input offset ->
                p1 addToParses input offset
                while parses.Count > 0 do
                    let struct (s, parse) = parses.Dequeue ()
                    sink s parse
                    parseFromSegment p2 addToNextBParses parse
                    for (struct (b, parse2)) in nextBParses do
                        parseFromSegment p1 addToNextAParses parse2
                        for (struct (a, parse3)) in nextAParses do
                            parses.Enqueue (f s b a, union (union parse parse2) parse3)
                        nextAParses.Clear ()
                    nextBParses.Clear ()


    let interleave1 (s : 'a -> 'b -> 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            let nextBParses = ResizeArray ()
            let addToNextBParses b parsed = struct (b, parsed) |> nextBParses.Add
            let nextAParses = ResizeArray ()
            let addToNextAParses a parsed = struct (a, parsed) |> nextAParses.Add

            let firstParseSink a1 parse =
                parseFromSegment p2 addToNextBParses parse
                for (struct (b, parse2)) in nextBParses do
                    parseFromSegment p1 addToNextAParses parse2
                    for (struct (a2, parse3)) in nextAParses do
                        struct (s a1 b a2, union (union parse parse2) parse3) |> parses.Enqueue
                    nextAParses.Clear ()
                nextBParses.Clear ()

            fun input offset ->
                p1 firstParseSink input offset
                while parses.Count > 0 do
                    let struct (s, parse) = parses.Dequeue ()
                    sink s parse
                    parseFromSegment p2 addToNextBParses parse
                    for (struct (b, parse2)) in nextBParses do
                        parseFromSegment p1 addToNextAParses parse2
                        for (struct (a, parse3)) in nextAParses do
                            parses.Enqueue (f s b a, union (union parse parse2) parse3)
                        nextAParses.Clear ()
                    nextBParses.Clear ()

    let ignore (p : 'a ParseFun) : unit ParseFun =
        fun sink ->
            let sink _ parsed = sink () parsed
            p sink

    let filter (f : 'a -> bool) (p : 'a ParseFun) : 'a ParseFun =
        fun sink ->
            let sink a parsed = if f a then sink a parsed
            p sink

    let cong (teq : Teq<'a, 'b>) : Teq<'a ParseFun, 'b ParseFun> =
        Teq.Cong.believeMe teq

    let rec make<'a> (p : 'a Parser) : 'a ParseFun =
        match p with
        | TextParser (tp, teq) ->
            tp |> textParser |> Teq.castFrom (cong teq)
        | Success a ->
            success a
        | Fail ->
            fail
        | Choice ps ->
            ps |> List.map make |> choice
        | Sequence crate ->
            crate.Apply
                { new ParserSequenceEval<_,_> with
                    member __.Eval f p1 p2 = sequence f (make p1) (make p2)
                }
        | Map crate ->
            crate.Apply
                { new ParserMapEval<_,_> with
                    member __.Eval f p = map f (make p)
                }
        | Bind crate ->
            crate.Apply
                { new ParserBindEval<_,_> with
                    member __.Eval f p = bind (f >> make) (make p)
                }
        | ZeroOrMore crate ->
            crate.Apply
                { new ParserZeroOrMoreEval<_,_> with
                    member __.Eval s f p = zeroOrMore s f (make p)
                }
        | OneOrMore crate ->
            crate.Apply
                { new ParserOneOrMoreEval<_,_> with
                    member __.Eval s f p = oneOrMore s f (make p)
                }
        | Interleave crate ->
            crate.Apply
                { new ParserInterleaveEval<_,_> with
                    member __.Eval s f p1 p2 = interleave s f (make p1) (make p2)
                }
        | Interleave1 crate ->
            crate.Apply
                { new ParserInterleave1Eval<_,_> with
                    member __.Eval s f p1 p2 = interleave1 s f (make p1) (make p2)
                }
        | Ignore (crate, teq) ->
            crate.Apply
                { new ParserEval<_> with
                    member __.Eval p = ignore (make p) |> Teq.castFrom (cong teq)
                }
        | Filter (f, p) ->
            filter f (make p)
