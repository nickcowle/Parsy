namespace Parsy

[<RequireQualifiedAccess>]
module ReferenceParser =

    val make<'a> : 'a Parser -> (string -> ('a * string) list)
