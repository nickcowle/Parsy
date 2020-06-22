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

    let make s offset length =
        {
            Value = s
            Offset = offset
            Length = length
        }

    let extend amount segment1 =
        { segment1 with Length = segment1.Length + amount }

    let current segment =
        segment.Value.Substring(segment.Offset, segment.Length)

    let remaining segment =
        segment.Value.Substring(segment.Offset + segment.Length)
