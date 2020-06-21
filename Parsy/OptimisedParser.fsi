namespace Parsy

[<RequireQualifiedAccess>]
module OptimisedParser =

    val make : 'a Parser -> (('a -> StringSegment -> unit) -> StringSegment -> unit)
