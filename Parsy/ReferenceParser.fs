namespace Parsy

open TypeEquality

[<RequireQualifiedAccess>]
module ReferenceParser =

    type 'a ParseFun = string -> ('a * string) list

    type ParserBuilder () =

        member __.Yield (a : 'a) : 'a ParseFun =
            fun input -> [ a, input ]

        member __.YieldFrom (p : 'a ParseFun) : 'a ParseFun =
            p

        member __.Bind (p : 'a ParseFun, f : 'a -> 'b ParseFun) : 'b ParseFun =
            fun input ->
                p input |> List.collect (fun (parsed, rest) -> f parsed rest)

        member __.Zero () : 'a ParseFun =
            fun _ -> []

        member __.For (xs : 'a list, f : 'a -> 'b ParseFun) : 'b ParseFun =
            let parsers = xs |> List.map f
            fun input -> parsers |> List.collect ((|>) input)

        member __.Combine (p1 : 'a ParseFun, p2 : 'a ParseFun) : 'a ParseFun =
            fun input -> p1 input @ p2 input

        member __.Delay (f : unit -> 'a ParseFun) : 'a ParseFun =
            f ()

    let parser = ParserBuilder ()

    let textParser (textParser : TextParser) : string ParseFun =
        ReferenceTextParser.make textParser

    let success (a : 'a) : 'a ParseFun =
        parser {
            yield a
        }

    let fail<'a> : 'a ParseFun =
        parser {
            ()
        }

    let choice (ps : 'a ParseFun list) : 'a ParseFun =
        parser {
            for p in ps do
                yield! p
        }

    let sequence (f : 'a -> 'b -> 'c) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 'c ParseFun =
        parser {
            let! a = p1
            let! b = p2
            yield f a b
        }

    let map (f : 'a -> 'b) (p : 'a ParseFun) : 'b ParseFun =
        parser {
            let! a = p
            yield f a
        }

    let bind (f : 'a -> 'b ParseFun) (p : 'a ParseFun) : 'b ParseFun =
        parser {
            let! a = p
            yield! f a
        }

    let rec zeroOrMore (s : 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        parser {
            yield s
            let! a = p
            yield! zeroOrMore (f s a) f p
        }

    let oneOrMore (s : 'a -> 's) (f : 's -> 'a -> 's) (p : 'a ParseFun) : 's ParseFun =
        parser {
            let! a = p
            yield! zeroOrMore (s a) f p
        }

    let rec interleaveInner (s : 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        parser {
            let! b = p2
            let! a = p1
            let s = f s b a
            yield s
            yield! interleaveInner s f p1 p2
        }

    let interleave (s : 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        parser {
            let! a = p1
            let s = s a
            yield s
            yield! interleaveInner s f p1 p2
        }

    let interleave1 (s : 'a -> 'b -> 'a -> 's) (f : 's -> 'b -> 'a -> 's) (p1 : 'a ParseFun) (p2 : 'b ParseFun) : 's ParseFun =
        parser {
            let! a = p1
            let! b = p2
            yield! interleave (s a b) f p1 p2
        }

    let ignore (p : 'a ParseFun) : unit ParseFun =
        parser {
            let! _ = p
            yield ()
        }

    let filter (f : 'a -> bool) (p : 'a ParseFun) : 'a ParseFun =
        parser {
            let! a = p
            if f a then yield a
        }

    let delay (f : unit -> 'a ParseFun) : 'a ParseFun =
        let parser = lazy f ()
        fun input -> parser.Value input

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
        | Delay f ->
            delay (f >> make)
