using System.Globalization;

using GetThePicture.Cobol.Picture;

namespace GetThePicture.Codec.Decoder.Semantic;

internal static class TimeDecoder
{
    public static TimeOnly Decode(string display, PicClause pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported TimeOnly base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Time' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        // TODO: 看看要不要支援 COMP-3 (PACKED-DECIMAL)

        return pic.Semantic switch
        {
            PicSemantic.Time6 => ParseTime6(display),
            PicSemantic.Time9 => ParseTime9(display),
            _ => throw new NotSupportedException($"Unsupported TimeOnly format: {pic.Semantic}")
        };
    }

    private static TimeOnly ParseTime6(string display)
    {
        // HHmmss
        if (!TimeOnly.TryParseExact(
                display,
                "HHmmss",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var time))
        {
            throw new FormatException($"Invalid Time6 DISPLAY value: '{display}'");
        }

        return time;
    }

    private static TimeOnly ParseTime9(string display)
    {
        // HHmmssfff
        if (!TimeOnly.TryParseExact(
                display,
                "HHmmssfff",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var time))
        {
            throw new FormatException($"Invalid Time9 DISPLAY value: '{display}'");
        }

        return time;
    }
}