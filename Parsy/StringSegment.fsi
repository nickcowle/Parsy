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

    val extend : StringSegment -> int -> StringSegment

    val current : StringSegment -> string

    val remaining : StringSegment -> string
