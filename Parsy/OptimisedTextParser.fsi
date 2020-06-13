namespace Parsy

[<Struct>]
type StringSegment =
    {
        Value : string
        Offset : int
        Length : int
    }


[<RequireQualifiedAccess>]
module StringSegment =

    val ofString : string -> StringSegment

    val advance : int -> StringSegment -> StringSegment

    val current : StringSegment -> string

    val consumed : StringSegment -> string

    val remaining : StringSegment -> string


[<RequireQualifiedAccess>]
module OptimisedTextParser =

    val make : TextParser -> ((StringSegment -> unit) -> StringSegment -> unit)
