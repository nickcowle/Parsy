namespace Parsy

open TypeEquality

[<RequireQualifiedAccess>]
module OptimisedParser =

    type 'a ParseFun = ('a -> StringSegment -> unit) -> StringSegment -> unit

    let textParser (textParser : TextParser) : string ParseFun =
        let textParser = textParser |> TextParserReducer.reduce
        fun sink -> OptimisedTextParser.make textParser (fun segment -> sink (segment |> StringSegment.current) segment)

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
