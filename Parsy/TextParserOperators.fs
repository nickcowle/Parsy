namespace Parsy

module TextParserOperators =

    let (~+) s = s |> TextParser.string |> TextParser.oneOrMore

    let (~-) s = s |> TextParser.string |> TextParser.zeroOrMore

    let (++) p1 p2 = TextParser.sequence p1 p2

    let (!) s = s |> TextParser.string

    let (!!) s = s |> Parser.ofTextParser |> Parser.ignore

    let (!!!) s = !!(!s)
