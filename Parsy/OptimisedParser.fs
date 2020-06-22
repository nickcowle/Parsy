namespace Parsy

open TypeEquality

[<RequireQualifiedAccess>]
module OptimisedParser =

    type 'a ParseFun = ('a -> StringSegment -> unit) -> StringSegment -> unit

    let textParser (textParser : TextParser) : string ParseFun =
        let textParser = textParser |> TextParserReducer.reduce
        fun sink ->
            let parser = OptimisedTextParser.make textParser (fun segment -> sink (segment |> StringSegment.current) segment)
            fun segment -> parser segment.Value (segment.Offset + segment.Length)

    let success (a : 'a) : 'a ParseFun =
        fun sink input ->
            sink a (input |> StringSegment.advance 0)

    let fail<'a> : 'a ParseFun =
        fun _ _ -> ()

    let choice (parsers : 'a ParseFun list) : 'a ParseFun =
        fun sink ->
            fun input ->
                for parser in parsers do
                    parser sink input

    let sequence (f : 'a -> 'b -> 'c) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 'c ParseFun =
        fun sink ->
            let p1Sink a segment =
                let p2Sink a b segment2 = sink (f a b) (segment |> StringSegment.extend segment2.Length)
                p2 (p2Sink a) segment
            p1 p1Sink

    let map (f : 'a -> 'b) (p : 'a ParseFun) : 'b ParseFun =
        fun sink ->
            let sink a segment = sink (f a) segment
            p sink

    let bind (f : 'a -> 'b ParseFun) (p : 'a ParseFun) : 'b ParseFun =
        fun sink ->
            fun input ->
                let sink a segment1 = f a (fun b segment2 -> sink b (segment1 |> StringSegment.extend segment2.Length)) segment1
                p sink input

    let rec zeroOrMore (s : 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            fun input ->
                parses.Enqueue struct (s, input |> StringSegment.advance 0)
                while parses.Count > 0 do
                    let struct (s, segment) = parses.Dequeue ()
                    sink s segment
                    p (fun a segment2 -> parses.Enqueue struct (f s a, segment |> StringSegment.extend segment2.Length)) segment

    and oneOrMore (s : 'a -> 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        fun sink ->
            let parses = System.Collections.Generic.Queue ()
            fun input ->
                p (fun a segment -> parses.Enqueue struct (s a, segment)) input
                while parses.Count > 0 do
                    let struct (s, segment) = parses.Dequeue ()
                    sink s segment
                    p (fun a segment2 -> parses.Enqueue struct (f s a, segment |> StringSegment.extend segment2.Length)) segment

    let interleave (s : 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        fun sink ->
            let rec p1Sink s allSoFarSegment =
                sink s allSoFarSegment
                let p2Sink b p2Segment =
                    p1 (fun a p1Segment -> p1Sink (f s b a) (allSoFarSegment |> StringSegment.extend (p1Segment.Length + p2Segment.Length))) p2Segment
                p2 p2Sink allSoFarSegment
            fun input ->
                p1 (fun a -> p1Sink (s a)) input

    let interleave1 (s : 'a -> 'b -> 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        fun sink ->
            let rec p1Sink s allSoFarSegment =
                sink s allSoFarSegment
                let p2Sink b p2Segment = p1 (fun a p1Segment -> p1Sink (f s b a) (allSoFarSegment |> StringSegment.extend (p1Segment.Length + p2Segment.Length))) p2Segment
                p2 p2Sink allSoFarSegment
            fun input ->
                p1 (fun a1 p1Segment -> p2 (fun b p2Segment -> p1 (fun a2 p1Segment2 -> p1Sink (s a1 b a2) (p1Segment |> StringSegment.extend (p2Segment.Length + p1Segment2.Length))) p2Segment) p1Segment) input

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
