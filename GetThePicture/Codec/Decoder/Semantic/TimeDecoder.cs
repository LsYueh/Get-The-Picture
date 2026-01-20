using System.Globalization;
using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Codec.Decoder.Semantic;

internal static class TimeDecoder
{
    public static TimeOnly Decode(ReadOnlySpan<byte> buffer, PicClause pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported TimeOnly base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Time' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        // TODO: 看看要不要支援 COMP-3 (PACKED-DECIMAL)

        return pic.Semantic switch
        {
            PicSemantic.Time6 => ParseTime6(buffer),
            PicSemantic.Time9 => ParseTime9(buffer),
            _ => throw new NotSupportedException($"Unsupported TimeOnly format: {pic.Semantic}")
        };
    }

    private static TimeOnly ParseTime6(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);
        
        // HHmmss
        if (!TimeOnly.TryParseExact(
                s, "HHmmss",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var time))
        {
            throw new FormatException($"Invalid Time6 DISPLAY value: '{s}'");
        }

        return time;
    }

    private static TimeOnly ParseTime9(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);

        // HHmmssfff
        if (!TimeOnly.TryParseExact(
                s, "HHmmssfff",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var time))
        {
            throw new FormatException($"Invalid Time9 DISPLAY value: '{s}'");
        }

        return time;
    }
}