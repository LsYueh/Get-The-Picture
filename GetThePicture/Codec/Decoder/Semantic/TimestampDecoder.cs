using System.Globalization;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Codec.Decoder.Semantic;

internal static class TimestampDecoder
{
    public static DateTime Decode(string display, PicClause pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported DateTime base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Timestamp' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

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
