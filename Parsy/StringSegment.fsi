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

    val make : string -> int -> int -> StringSegment

    val ofString : string -> StringSegment

    val advance : int -> StringSegment -> StringSegment

    val extend : int -> StringSegment -> StringSegment

    val current : StringSegment -> string

    val remaining : StringSegment -> string
