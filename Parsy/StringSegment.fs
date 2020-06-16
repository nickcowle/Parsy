﻿namespace Parsy

[<Struct>]
type StringSegment =
    {
        Value : string
        Offset : int
        Length : int
    }


[<RequireQualifiedAccess>]
module StringSegment =

    let ofString s =
        {
            Value = s
            Offset = 0
            Length = 0
        }

    let advance amount segment =
        {
            Value = segment.Value
            Offset = segment.Offset + segment.Length
            Length = amount
        }

    let extend segment1 amount =
        { segment1 with Length = segment1.Length + amount }

    let current segment =
        segment.Value.Substring(segment.Offset, segment.Length)

    let remaining segment =
        segment.Value.Substring(segment.Offset + segment.Length)