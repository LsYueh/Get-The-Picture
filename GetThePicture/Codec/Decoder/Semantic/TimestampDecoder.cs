using System.Globalization;

using GetThePicture.Cobol.Picture;

namespace GetThePicture.Codec.Decoder.Semantic;

internal static class TimestampDecoder
{
    public static DateTime Decode(string display, PicClause pic)
    {
        if (pic.BaseType == PicBaseType.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported DateTime base type: PIC S9");

        return pic.Semantic switch
        {
            PicSemantic.Timestamp14 => ParseTimestamp14(display),
            _ => throw new NotSupportedException($"Unsupported DateTime format: {pic.Semantic}")
        };
    }

    private static DateTime ParseTimestamp14(string display)
    {
        // yyyyMMddHHmmss
        if (!DateTime.TryParseExact(
                display,
                "yyyyMMddHHmmss",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var dt))
        {
            throw new FormatException($"Invalid Timestamp14 DISPLAY value: '{display}'");
        }

        return dt;
    }
}
