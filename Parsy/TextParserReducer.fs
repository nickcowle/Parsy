namespace Parsy

[<RequireQualifiedAccess>]
module TextParserReducer =

    let rec reduce (parser : TextParser) : TextParser =
        match parser with
        | Choice ps ->
            let rec addChoices p (choices, success) =
                let p = reduce p
                match p with
                | Fail -> choices, success
                | Success -> choices, true
                | Choice ps -> List.foldBack addChoices ps (choices, success)
                | _ -> p::choices, success
            let ps, success = List.foldBack addChoices ps ([], false)
            let ps = if success then Success :: ps else ps
            match ps with
            | [] -> Fail
            | [p] -> p
            | _ -> Choice ps
        | Sequence (p1, p2) ->
            let p1 = reduce p1
            let p2 = reduce p2
            match p1, p2 with
            | Fail, _ -> Fail
            | _, Fail -> Fail
            | Success, _ -> p2
            | _, Success -> p1
            | String s1, String s2 -> String (sprintf "%s%s" s1 s2)
            | String s, Character c -> String (sprintf "%s%c" s c)
            | Character c, String s -> String (sprintf "%c%s" c s)
            | Character c1, Character c2 -> String (sprintf "%c%c" c1 c2)
            | _ -> Sequence (p1, p2)
        | ZeroOrMore p ->
            let p = reduce p
            match p with
            | Fail -> Success
            | _ -> ZeroOrMore p
        | OneOrMore p ->
            let p = reduce p
            match p with
            | Fail -> Fail
            | _ -> OneOrMore p
        | Bind (f, p) ->
            let p = reduce p
            match p with
            | Success -> f "" |> reduce
            | Fail -> Fail
            | _ -> Bind (f >> reduce, p)
        | Filter (f, p) ->
            let p = reduce p
            match p with
            | Fail -> Fail
            | Filter (f2, p2) ->
                let f s = f2 s && f s
                Filter (f, p2)
            | _ -> Filter (f, p)
        | Delay f ->
            Delay (f >> reduce)
        | Success
        | Fail
        | Character _
        | String _
        | Letter
        | Digit
        | LetterOrDigit ->
            parser
