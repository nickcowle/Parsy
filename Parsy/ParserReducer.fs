namespace Parsy

open TypeEquality

[<RequireQualifiedAccess>]
module ParserReducer =

    let rec reduce<'a> (p : 'a Parser) : 'a Parser =
        match p with
        | TextParser (tp, teq) ->
            let tp = TextParserReducer.reduce tp
            match tp with
            | TextParser.Success -> Parser.success ("" |> Teq.castFrom teq)
            | TextParser.Fail -> Parser.fail
            | _ -> TextParser (tp, teq)
        | Success _ -> p
        | Fail -> p
        | Choice ps ->
            let rec addChoices p choices =
                let p = reduce p
                match p with
                | Fail -> choices
                | Choice ps -> List.foldBack addChoices ps choices
                | _ -> p::choices
            let ps = List.foldBack addChoices ps []
            match ps with
            | [] -> Parser.fail
            | [p] -> p
            | _ -> Parser.choice ps
        | Sequence crate ->
            crate.Apply
                { new ParserSequenceEval<_,_> with
                    member __.Eval f p1 p2 =
                        let p1 = reduce p1
                        let p2 = reduce p2
                        match p1, p2 with
                        | Fail, _ -> Parser.fail
                        | _, Fail -> Parser.fail
                        | Success a, _ -> Parser.map (f a) p2 |> reduce
                        | _, Success b -> Parser.map (fun a -> f a b) p1 |> reduce
                        | _ -> Parser.sequence f p1 p2
                }
        | Map crate ->
            crate.Apply
                { new ParserMapEval<_,_> with
                    member __.Eval f p =
                        let p = reduce p
                        match p with
                        | Fail -> Parser.fail
                        | Success a -> Parser.success (f a)
                        | _ -> Parser.map f p
                }
        | Bind crate ->
            crate.Apply
                { new ParserBindEval<_,_> with
                    member __.Eval f p =
                        let p = reduce p
                        let f = f >> reduce
                        match p with
                        | Fail -> Parser.fail
                        | Success a -> f a
                        | _ -> Parser.bind f p
                }
        | ZeroOrMore crate ->
            crate.Apply
                { new ParserZeroOrMoreEval<_,_> with
                    member __.Eval s f p =
                        let p = reduce p
                        match p with
                        | Fail -> Parser.success s
                        | _ -> Parser.zeroOrMore s f p
                }
        | OneOrMore crate ->
            crate.Apply
                { new ParserOneOrMoreEval<_,_> with
                    member __.Eval s f p =
                        let p = reduce p
                        match p with
                        | Fail -> Parser.fail
                        | _ -> Parser.oneOrMore s f p
                }
        | Interleave crate ->
            crate.Apply
                { new ParserInterleaveEval<_,_> with
                    member __.Eval s f p1 p2 =
                        let p1 = reduce p1
                        let p2 = reduce p2
                        match p1, p2 with
                        | Fail, _ -> Parser.fail
                        | Success a, _ -> Parser.zeroOrMore (s a) (fun s b -> f s b a) p2 |> reduce
                        | _, Fail -> Parser.map s p1 |> reduce
                        | _, Success b -> Parser.oneOrMore s (fun s a -> f s b a) p1 |> reduce
                        | _ -> Parser.interleave s f p1 p2
                }
        | Interleave1 crate ->
            crate.Apply
                { new ParserInterleave1Eval<_,_> with
                    member __.Eval s f p1 p2 =
                        let p1 = reduce p1
                        let p2 = reduce p2
                        match p1, p2 with
                        | Fail, _ -> Parser.fail
                        | Success a, _ -> Parser.oneOrMore (fun b -> s a b a) (fun s b -> f s b a) p2 |> reduce
                        | _, Fail -> Parser.Fail
                        | _ -> Parser.interleave1 s f p1 p2
                }
        | Ignore (crate, teq) ->
            crate.Apply
                { new ParserEval<_> with
                    member __.Eval p =
                        let p = reduce p
                        match p with
                        | Fail -> Parser.fail
                        | Success _ -> Parser.success (() |> Teq.castFrom teq)
                        | _ ->
                            let crate = { new ParserCrate with member __.Apply e = e.Eval p }
                            Ignore (crate, teq)
                }
        | Filter (f, p) ->
            let p = reduce p
            match p with
            | Fail -> Parser.fail
            | _ -> Parser.filter f p
        | Delay f ->
            Parser.delay (f >> reduce)
