namespace Parsy

[<RequireQualifiedAccess>]
module String =

    let startsWith (test : string) (s : string) =
        System.String.CompareOrdinal(s, 0, test, 0, test.Length) = 0
