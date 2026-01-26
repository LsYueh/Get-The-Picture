using System.Globalization;
using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause.Decoder.Semantic;

internal static class TimestampDecoder
{
    public static DateTime Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported DateTime base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Timestamp' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        return pic.Semantic switch
        {
            PicSemantic.Timestamp14 => ParseTimestamp14(buffer),
            _ => throw new NotSupportedException($"Unsupported DateTime format: {pic.Semantic}")
        };
    }

    private static DateTime ParseTimestamp14(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);

        // yyyyMMddHHmmss
        if (!DateTime.TryParseExact(
                s, "yyyyMMddHHmmss",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var dt))
        {
            throw new FormatException($"Invalid Timestamp14 value: '{s}'");
        }

        return dt;
    }
}
