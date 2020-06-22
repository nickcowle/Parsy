namespace Parsy

open TypeEquality

[<NoEquality>]
[<NoComparison>]
type 'a Parser =
    internal
    | TextParser of TextParser * Teq<'a, string>
    | Success of 'a
    | Fail
    | Choice of 'a Parser list
    | Sequence of 'a ParserSequenceCrate
    | Map of 'a ParserMapCrate
    | Bind of 'a ParserBindCrate
    | ZeroOrMore of 'a ParserZeroOrMoreCrate
    | OneOrMore of 'a ParserOneOrMoreCrate
    | Interleave of 'a ParserInterleaveCrate
    | Interleave1 of 'a ParserInterleave1Crate
    | Ignore of ParserCrate * Teq<'a, unit>

and internal 'a ParserSequenceCrate = abstract Apply : ParserSequenceEval<'a, 'ret> -> 'ret
and internal ParserSequenceEval<'a, 'ret> = abstract Eval : ('b -> 'c -> 'a) -> 'b Parser -> 'c Parser -> 'ret

and internal 'a ParserMapCrate = abstract Apply : ParserMapEval<'a, 'ret> -> 'ret
and internal ParserMapEval<'a, 'ret> = abstract Eval : ('b -> 'a) -> 'b Parser -> 'ret

and internal 'a ParserBindCrate = abstract Apply : ParserBindEval<'a, 'ret> -> 'ret
and internal ParserBindEval<'a, 'ret> = abstract Eval : ('b -> 'a Parser) -> 'b Parser -> 'ret

and internal 's ParserZeroOrMoreCrate = abstract Apply : ParserZeroOrMoreEval<'s, 'ret> -> 'ret
and internal ParserZeroOrMoreEval<'s, 'ret> = abstract Eval : 's -> ('s -> 'a -> 's) -> 'a Parser -> 'ret

and internal 's ParserOneOrMoreCrate = abstract Apply : ParserOneOrMoreEval<'s, 'ret> -> 'ret
and internal ParserOneOrMoreEval<'s, 'ret> = abstract Eval : ('a -> 's) -> ('s -> 'a -> 's) -> 'a Parser -> 'ret

and internal 's ParserInterleaveCrate = abstract Apply : ParserInterleaveEval<'s, 'ret> -> 'ret
and internal ParserInterleaveEval<'s, 'ret> = abstract Eval : ('a -> 's) -> ('s -> 'b -> 'a -> 's) -> 'a Parser -> 'b Parser -> 'ret

and internal 's ParserInterleave1Crate = abstract Apply : ParserInterleave1Eval<'s, 'ret> -> 'ret
and internal ParserInterleave1Eval<'s, 'ret> = abstract Eval : ('a -> 'b -> 'a -> 's) -> ('s -> 'b -> 'a -> 's) -> 'a Parser -> 'b Parser -> 'ret

and internal ParserCrate = abstract Apply : ParserEval<'ret> -> 'ret
and internal ParserEval<'ret> = abstract Eval : 'a Parser -> 'ret


[<RequireQualifiedAccess>]
module Parser =

    let ofTextParser textParser = TextParser (textParser, Teq.refl)

    let success = Success

    let fail = Fail

    let choice = Choice

    let sequence f p1 p2 =
        { new obj () with
            member __.ToString () = sprintf "%A" (f, p1, p2)
        interface ParserSequenceCrate<_> with
            member __.Apply e = e.Eval f p1 p2
        }
        |> Sequence

    let map f p =
        { new obj () with
            member __.ToString () = sprintf "%A" (f, p)
        interface ParserMapCrate<_> with
            member __.Apply e = e.Eval f p
        }
        |> Map

    let bind f p =
        { new obj () with
            member __.ToString () = sprintf "%A" (f, p)
        interface ParserBindCrate<_> with
            member __.Apply e = e.Eval f p
        }
        |> Bind

    let zeroOrMore s f p =
        { new obj () with
            member __.ToString () = sprintf "%A" (s, f, p)
        interface ParserZeroOrMoreCrate<_> with
            member __.Apply e = e.Eval s f p
        }
        |> ZeroOrMore

    let oneOrMore s f p =
        { new obj () with
            member __.ToString () = sprintf "%A" (s, f, p)
        interface ParserOneOrMoreCrate<_> with
            member __.Apply e = e.Eval s f p
        }
        |> OneOrMore

    let interleave s f p1 p2 =
        { new obj () with
            member __.ToString () = sprintf "%A" (s, f, p1, p2)
        interface ParserInterleaveCrate<_> with
            member __.Apply e = e.Eval s f p1 p2
        }
        |> Interleave

    let interleave1 s f p1 p2 =
        { new obj () with
            member __.ToString () = sprintf "%A" (s, f, p1, p2)
        interface ParserInterleave1Crate<_> with
            member __.Apply e = e.Eval s f p1 p2
        }
        |> Interleave1

    let ignore p =
        let crate =
            { new obj () with
                member __.ToString () = sprintf "%A" p
            interface ParserCrate with
                member __.Apply e = e.Eval p
            }
        Ignore (crate, Teq.refl)

    let optional p = choice [ success None ; map Some p ]
