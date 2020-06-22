namespace Parsy

module ParserOperators =

    val (.>>) : 'a Parser -> 'b Parser -> 'a Parser

    val (>>.) : 'a Parser -> 'b Parser -> 'b Parser

    val (.>>.) : 'a Parser -> 'b Parser -> ('a * 'b) Parser

    val (>=>) : 'a Parser -> ('a -> 'b) -> 'b Parser

    val (>==>) : ('a Parser * 'b Parser) -> ('a -> 'b -> 'c) -> 'c Parser

    val (>===>) : ('a Parser * 'b Parser * 'c Parser) -> ('a -> 'b -> 'c -> 'd) -> 'd Parser

    val (>====>) : ('a Parser * 'b Parser * 'c Parser * 'd Parser) -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e Parser
