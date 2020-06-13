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

    let success = Success

    let fail = Fail

    let choice = Choice

    let sequence p1 p2 = Sequence (p1, p2)

    let zeroOrMore = ZeroOrMore

    let oneOrMore = OneOrMore

    let bind f p = Bind (f, p)

    let filter f p = Filter (f, p)

    let delay = Delay

    let character = Character

    let string = String

    let letter = Letter

    let digit = Digit

    let letterOrDigit = LetterOrDigit
