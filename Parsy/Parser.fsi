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
    | Filter of ('a -> bool) * 'a Parser

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

    val ofTextParser : TextParser -> string Parser

    val success : 'a -> 'a Parser

    val fail : 'a Parser

    val choice : 'a Parser list -> 'a Parser

    val sequence : ('a -> 'b -> 'c) -> 'a Parser -> 'b Parser -> 'c Parser

    val map : ('a -> 'b) -> 'a Parser -> 'b Parser

    val bind : ('a -> 'b Parser) -> 'a Parser -> 'b Parser

    val zeroOrMore : 's -> ('s -> 'a -> 's) -> 'a Parser -> 's Parser

    val oneOrMore : ('a -> 's) -> ('s -> 'a -> 's) -> 'a Parser -> 's Parser

    val interleave : ('a -> 's) -> ('s -> 'b -> 'a -> 's) -> 'a Parser -> 'b Parser -> 's Parser

    val interleave1 : ('a -> 'b -> 'a -> 's) -> ('s -> 'b -> 'a -> 's) -> 'a Parser -> 'b Parser -> 's Parser

    val ignore : 'a Parser -> unit Parser

    val optional : 'a Parser -> 'a option Parser

    val filter : ('a -> bool) -> 'a Parser -> 'a Parser
