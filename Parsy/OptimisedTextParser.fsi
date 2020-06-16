namespace Parsy

[<RequireQualifiedAccess>]
module OptimisedTextParser =

    val make : TextParser -> ((StringSegment -> unit) -> StringSegment -> unit)
