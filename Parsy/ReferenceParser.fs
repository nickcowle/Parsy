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
