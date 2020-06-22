namespace Parsy

module TextParserOperators =

    val (~+) : string -> TextParser

    val (~-) : string -> TextParser

    val (++) : TextParser -> TextParser -> TextParser

    val (!)  : string -> TextParser

    val (!!) : TextParser -> unit Parser

    val (!!!) : string -> unit Parser
