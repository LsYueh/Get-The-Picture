using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Decoder.Semantic;

internal static class DateDecoder
{
    public static DateOnly Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported DateOnly base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Date' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        // TODO: 看看要不要支援 COMP-3 (PACKED-DECIMAL)

        return pic.Semantic switch
        {
            PicSemantic.GregorianDate => ParseGregorianDate(buffer),
            PicSemantic.MinguoDate    => ParseMinguoDate(buffer),
            _ => throw new NotSupportedException($"Unsupported DateOnly format: {pic.Semantic}")
        };
    }

    private static DateOnly ParseGregorianDate(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);

        if (!DateOnly.TryParseExact(
                s, "yyyyMMdd",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var date))
        {
            throw new FormatException($"Invalid Gregorian date value: '{s}'");
        }

        return date;
    }

    private static DateOnly ParseMinguoDate(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);
        
        if (s.Length < 7)
        {
            throw new FormatException($"Invalid Minguo date value: '{s}'");
        }

        // 前 3 碼：民國年
        if (!int.TryParse(s[..3], out int minguoYear) ||
            !int.TryParse(s[3..5], out int month) ||
            !int.TryParse(s[5..7], out int day))
        {
            throw new FormatException($"Invalid Minguo date value: '{s}'");
        }

        int gregorianYear = minguoYear + 1911;

        try
        {
            return new DateOnly(gregorianYear, month, day);
        }
        catch (ArgumentOutOfRangeException ex)
        {
            throw new FormatException($"Invalid Minguo date value: '{s}'", ex);
        }
    }
}