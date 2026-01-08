using System.Globalization;
using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolPicEecoder
{
    /// <summary>
    /// CLR value → Display Value → COBOL PICTURE DISPLAY
    /// </summary>
    public static string Encode(object value, PicClause pic, CodecOptions codecOptions)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        // TODO: DisplayValue Refact...

        byte[] cp950Bytes = EncodeToLogicalBytes(value, pic);

        return EncodeBaseType(cp950Bytes, pic, codecOptions);
    }

    private static string EncodeBaseType(byte[] cp950Bytes, PicClause pic, CodecOptions codecOptions)
    {
        return pic.BaseType switch
        {
            PicBaseType.Numeric      =>      CobolNumericEncoder.Encode(cp950Bytes, pic, codecOptions),
            PicBaseType.Alphanumeric => CobolAlphanumericEncoder.Encode(cp950Bytes, pic),
            PicBaseType.Alphabetic   =>   CobolAlphabeticEncoder.Encode(cp950Bytes, pic),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseType}"),
        };
    }

    private static byte[] EncodeToLogicalBytes(object value, PicClause pic)
    {
        if (value == null) return [];

        string text = ConvertToLogicalText(value, pic);

        Encoding cp950 = EncodingFactory.CP950;

        return cp950.GetBytes(text);
    }

    /// <summary>
    /// 底層的型別 → 文字 (要跑Test)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    internal static string ConvertToLogicalText(object value, PicClause pic)
    {
        return value switch
        {
            string s => s,
            char   c => c.ToString(),
            sbyte or byte or short or ushort or int or uint or long or ulong => FormatIntegerInvariant(value),
            float or double or decimal => FormatDecimalInvariant(value, pic),
            DateOnly d => ConvertDateOnlyToText(d, pic),
            TimeOnly t => ConvertTimeOnlyToText(t, pic),
            DateTime dt => ConvertDateTimeToText(dt, pic),
            _ => throw new NotSupportedException($"Unsupported value type '{value.GetType()}'"),
        };
    }

    private static string FormatIntegerInvariant(object value)
    {
        if (value is IFormattable formattable)
            return formattable.ToString(null, CultureInfo.InvariantCulture);

        return value.ToString()!;
    }

    private static string FormatDecimalInvariant(object value, PicClause pic)
    {
        ArgumentNullException.ThrowIfNull(value);
        
        decimal d = value switch
        {
            float   f   => (decimal)f,
            double  db  => (decimal)db,
            decimal dec => dec,
            _ => throw new ArgumentException("Value must be a floating point type", nameof(value)),
        };

        if (pic.DecimalDigits == 0)
        {
            if (d != decimal.Truncate(d))
                throw new InvalidOperationException("Value has fraction but expected integer");
            return decimal.Truncate(d).ToString("0", CultureInfo.InvariantCulture);
        }

        // scale decimal
        decimal scaled = d * Pow10(pic.DecimalDigits);

        if (scaled != decimal.Truncate(scaled))
            throw new InvalidOperationException("Value exceeds allowed precision");

        return decimal.Truncate(scaled).ToString("0", CultureInfo.InvariantCulture);
    }

    private static decimal Pow10(int n)
    {
        decimal result = 1m;
        for (int i = 0; i < n; i++)
            result *= 10m;
        return result;
    }

    private static string ConvertDateOnlyToText(DateOnly date, PicClause pic)
    {
        return pic.Semantic switch
        {
            PicSemantic.GregorianDate => date.ToString("yyyyMMdd"),
            PicSemantic.MinguoDate => ToMinguoDateString(date),
            _ => throw new NotSupportedException($"Unsupported DateOnly format: {pic.Semantic}")
        };
    }

    private static string ToMinguoDateString(DateOnly date)
    {
        int rocYear = date.Year - 1911;
        
        if (rocYear <= 0)
        {
            throw new ArgumentOutOfRangeException(nameof(date), "Date is before ROC calendar starts (1912-01-01).");
        }

        // yyyMMdd
        return $"{rocYear:000}{date:MMdd}";
    }

    private static string ConvertTimeOnlyToText(TimeOnly dt, PicClause pic)
    {
        return pic.Semantic switch
        {
            PicSemantic.Time6 => dt.ToString("HHmmss"   , CultureInfo.InvariantCulture),
            PicSemantic.Time9 => dt.ToString("HHmmssfff", CultureInfo.InvariantCulture),
            _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
        };
    }

    private static string ConvertDateTimeToText(DateTime dt, PicClause pic)
    {
        if (pic.Semantic != PicSemantic.Timestamp14)
        {
            throw new NotSupportedException($"DateTime can only be encoded as Timestamp14, but was {pic.Semantic}");
        }

        return dt.ToString("yyyyMMddHHmmss", CultureInfo.InvariantCulture);
    }
}