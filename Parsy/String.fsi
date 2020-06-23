namespace Parsy

[<RequireQualifiedAccess>]
module String =

    /// We define our own version of String.startsWith to ensure
    /// consistent results across Windows and Linux.
    /// For example, "ab".StartsWith("a\031") is false on Windows,
    /// but true on Linux.
    /// In this custom version, however, it is false on both platforms.
    val startsWith : string -> string -> bool

    val endsWith : string -> string -> bool
