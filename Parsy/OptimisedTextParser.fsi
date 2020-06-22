namespace Parsy

[<RequireQualifiedAccess>]
module OptimisedTextParser =

    val make : TextParser -> ((StringSegment -> unit) -> string -> int -> unit)
