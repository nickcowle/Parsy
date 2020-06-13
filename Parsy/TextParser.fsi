namespace Parsy

[<NoEquality>]
[<NoComparison>]
type TextParser =
    internal
    | Success
    | Fail
    | Choice of TextParser list
    | Sequence of TextParser * TextParser
    | ZeroOrMore of TextParser
    | OneOrMore of TextParser
    | Bind of (string -> TextParser) * TextParser
    | Filter of (string -> bool) * TextParser
    | Delay of (unit -> TextParser)
    | Character of char
    | String of string
    | Letter
    | Digit
    | LetterOrDigit


[<RequireQualifiedAccess>]
module TextParser =

    val success : TextParser

    val fail : TextParser

    val choice : TextParser list -> TextParser

    val sequence : TextParser -> TextParser -> TextParser

    val zeroOrMore : TextParser -> TextParser

    val oneOrMore : TextParser -> TextParser

    val bind : (string -> TextParser) -> TextParser -> TextParser

    val filter : (string -> bool) -> TextParser -> TextParser

    val delay : (unit -> TextParser) -> TextParser

    val character : char -> TextParser

    val string : string -> TextParser

    val letter : TextParser

    val digit : TextParser

    val letterOrDigit : TextParser
