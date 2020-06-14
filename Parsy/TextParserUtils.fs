namespace Parsy

[<RequireQualifiedAccess>]
module TextParserUtils =

    let private combineResults (res1 : bool option) (res2 : bool option) : bool option =
        match res1, res2 with
        | Some true,  Some true  -> Some true
        | Some false, _          -> Some false
        | _,          Some false -> Some false
        | _                      -> None

    let rec tryCompare (parser1 : TextParser) (parser2 : TextParser) : bool option =
        match parser1, parser2 with
        | Success,           Success           -> Some true
        | Fail,              Fail              -> Some true
        | Choice parsers1,   Choice parsers2   -> tryCompareParsers parsers1 parsers2
        | Sequence (p1, p2), Sequence (p3, p4) -> combineResults (tryCompare p1 p3) (tryCompare p2 p4)
        | ZeroOrMore p1,     ZeroOrMore p2     -> tryCompare p1 p2
        | OneOrMore p1,      OneOrMore p2      -> tryCompare p1 p2
        | Bind (_, p1),      Bind (_, p2)      -> match tryCompare p1 p2 with Some false -> Some false | _ -> None
        | Filter (_, p1),    Filter (_, p2)    -> match tryCompare p1 p2 with Some false -> Some false | _ -> None
        | Delay _,           Delay _           -> None
        | Character c1,      Character c2      -> Some (c1 = c2)
        | String s1,         String s2         -> Some (s1 = s2)
        | Letter,            Letter            -> Some true
        | Digit,             Digit             -> Some true
        | LetterOrDigit,     LetterOrDigit     -> Some true
        | _ -> Some false

    and private tryCompareParsers (ps1 : TextParser list) (ps2 : TextParser list) : bool option =
        if ps1 |> List.length <> (ps2 |> List.length) then
            Some false
        else
            let results = List.map2 tryCompare ps1 ps2
            if results |> List.forall ((=) (Some true)) then
                Some true
            else if results |> List.exists ((=) (Some false)) then
                Some false
            else
                None
