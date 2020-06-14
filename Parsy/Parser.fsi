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

    val ofTextParser : TextParser -> string Parser

    val success : 'a -> 'a Parser

    val fail : 'a Parser

    val choice : 'a Parser list -> 'a Parser

    val sequence : ('a -> 'b -> 'c) -> 'a Parser -> 'b Parser -> 'c Parser

    val map : ('a -> 'b) -> 'a Parser -> 'b Parser
