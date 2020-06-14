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

and internal 'a ParserSequenceCrate = abstract Apply : ParserSequenceEval<'a, 'ret> -> 'ret
and internal ParserSequenceEval<'a, 'ret> = abstract Eval : ('b -> 'c -> 'a) -> 'b Parser -> 'c Parser -> 'ret

and internal 'a ParserMapCrate = abstract Apply : ParserMapEval<'a, 'ret> -> 'ret
and internal ParserMapEval<'a, 'ret> = abstract Eval : ('b -> 'a) -> 'b Parser -> 'ret


[<RequireQualifiedAccess>]
module Parser =

    let ofTextParser textParser = TextParser (textParser, Teq.refl)

    let success = Success

    let fail = Fail

    let choice = Choice

    let sequence f p1 p2 =
        { new ParserSequenceCrate<_> with
            member __.Apply e = e.Eval f p1 p2
        }
        |> Sequence

    let map f p =
        { new ParserMapCrate<_> with
            member __.Apply e = e.Eval f p
        }
        |> Map
