namespace Parsy

module ParserOperators =

    let (>=>) p f = Parser.map f p

    let (>==>) (p1, p2) f = Parser.sequence f p1 p2

    let (>===>) (p1, p2, p3) f = Parser.sequence (<|) (Parser.sequence f p1 p2) p3

    let (>====>) (p1, p2, p3, p4) f = Parser.sequence (<|) ((p1, p2, p3) >===> f) p4

    let (.>>) p1 p2 = (p1, p2 |> Parser.ignore) >==> (fun a () -> a)

    let (>>.) p1 p2 = (p1 |> Parser.ignore, p2) >==> (fun () b -> b)

    let (.>>.) p1 p2 = (p1, p2) >==> (fun a b -> a, b)
