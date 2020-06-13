namespace Parsy

[<RequireQualifiedAccess>]
module ReferenceTextParser =

    val make : TextParser -> (string -> (string * string) list)
