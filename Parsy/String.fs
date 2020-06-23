namespace Parsy

[<RequireQualifiedAccess>]
module String =

    let startsWith (test : string) (s : string) =
        System.String.CompareOrdinal(s, 0, test, 0, test.Length) = 0

    let endsWith (test : string) (s : string) =
        s.Length >= test.Length
        && System.String.CompareOrdinal(s, s.Length - test.Length, test, 0, test.Length) = 0
