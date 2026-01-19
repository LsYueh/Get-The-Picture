using System.Globalization;

using GetThePicture.Cobol.Picture;

namespace GetThePicture.Codec.Decoder.Semantic;

internal static class DateDecoder
{
    public static DateOnly Decode(string display, PicClause pic)
    {
        if (pic.BaseClass == PicBaseClass.Numeric && pic.Signed)
            throw new NotSupportedException($"Unsupported DateOnly base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Date' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        // TODO: 看看要不要支援 COMP-3 (PACKED-DECIMAL)

        return pic.Semantic switch
        {
            PicSemantic.GregorianDate => ParseGregorianDate(display),
            PicSemantic.MinguoDate    => ParseMinguoDate(display),
            _ => throw new NotSupportedException($"Unsupported DateOnly format: {pic.Semantic}")
        };
    }

    private static DateOnly ParseGregorianDate(string display)
    {
        if (!DateOnly.TryParseExact(
                display,
                "yyyyMMdd",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var date))
        {
            throw new FormatException($"Invalid Gregorian date DISPLAY value: '{display}'");
        }

        return date;
    }

    private static DateOnly ParseMinguoDate(string display)
    {
        if (display.Length < 7)
        {
            throw new FormatException($"Invalid Minguo date DISPLAY value: '{display}'");
        }

        // 前 3 碼：民國年
        if (!int.TryParse(display[..3], out int minguoYear) ||
            !int.TryParse(display[3..5], out int month) ||
            !int.TryParse(display[5..7], out int day))
        {
            throw new FormatException($"Invalid Minguo date DISPLAY value: '{display}'");
        }

        int gregorianYear = minguoYear + 1911;

        try
        {
            return new DateOnly(gregorianYear, month, day);
        }
        catch (ArgumentOutOfRangeException ex)
        {
            throw new FormatException($"Invalid Minguo date DISPLAY value: '{display}'", ex);
        }
    }
}