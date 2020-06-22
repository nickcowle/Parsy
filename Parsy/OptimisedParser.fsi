namespace Parsy

[<RequireQualifiedAccess>]
module OptimisedParser =

    val make : 'a Parser -> (('a -> StringSegment -> unit) -> string -> int -> unit)
