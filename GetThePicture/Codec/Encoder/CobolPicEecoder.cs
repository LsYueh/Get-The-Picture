using System.Globalization;
using System.Text;

using GetThePicture.Cobol.Display;
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

        DisplayValue displayValue = ToDisplayValue(value, pic);

        return pic.BaseType switch
        {
            // PicBaseType.Numeric      =>      CobolNumericEncoder.Encode(displayValue, pic, codecOptions),
            // PicBaseType.Alphanumeric => CobolAlphanumericEncoder.Encode(displayValue, pic),
            // PicBaseType.Alphabetic   =>   CobolAlphabeticEncoder.Encode(displayValue, pic),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseType}"),
        };
    }

    /// <summary>
    /// CLR value → Display Value (要跑Test)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    internal static DisplayValue ToDisplayValue(object value, PicClause pic)
    {
        DisplayValue displayValue = value switch
        {
            string s => DvString(s),
            char   c => DvString(c.ToString()),
            sbyte or byte or short or ushort or int or uint or long or ulong => DvInteger(value),
            float or double or decimal => DvDecimal(value, pic),
            DateOnly d => DvDateOnly(d, pic),
            TimeOnly t => DvTimeOnly(t, pic),
            DateTime dt => DvDateTime(dt, pic),
            _ => throw new NotSupportedException($"Unsupported value type '{value.GetType()}'"),
        };

        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Text,   Text:   { } t } => t.Value,
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}'"),
        };

        byte[] raw = cp950.GetBytes(text);
        displayValue.SetRaw(raw);

        return displayValue;
    }

    private static DisplayValue DvString(string text)
    {
        return DisplayValue.FromText(text);
    }

    private static DisplayValue DvInteger(object value)
    {
        long number = value switch
        {
            sbyte v  => v,
            byte v   => v,
            short v  => v,
            ushort v => v,
            int v    => v,
            uint v   => v,
            long v   => v,
            ulong v  => checked((long)v),

            _ => throw new NotSupportedException($"Unsupported integer type '{value.GetType()}'")
        };

        bool isNegative = number < 0;
        string digits = Math.Abs(number).ToString(CultureInfo.InvariantCulture);

        return DisplayValue.FromNumber(isNegative, digits, decimalDigits: 0);
    }

    private static DisplayValue DvDecimal(object value, PicClause pic)
    {
        ArgumentNullException.ThrowIfNull(value);
        
        decimal d = value switch
        {
            float   f   => (decimal)f,
            double  db  => (decimal)db,
            decimal dec => dec,

            _ => throw new ArgumentException("Value must be a floating point type", nameof(value)),
        };

        bool isNegative = d < 0;

        d = Math.Abs(d); // 去掉sign

        int scale = pic.DecimalDigits;
        decimal scaled = d * Pow10(scale);
        
        if (scaled != decimal.Truncate(scaled))
            throw new InvalidOperationException("Value exceeds allowed precision");

        string digits = decimal.Truncate(scaled).ToString("0", CultureInfo.InvariantCulture);

        return DisplayValue.FromNumber(isNegative, digits, scale);
    }

    private static decimal Pow10(int n)
    {
        decimal result = 1m;
        for (int i = 0; i < n; i++)
            result *= 10m;
        return result;
    }

    private static DisplayValue DvDateOnly(DateOnly date, PicClause pic)
    {
        return pic.Semantic switch
        {
            PicSemantic.GregorianDate => DisplayValue.FromNumber(date.ToString("yyyyMMdd")),
            PicSemantic.MinguoDate => DisplayValue.FromNumber(ToMinguoDateString(date)),
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

        return $"{rocYear:000}{date:MMdd}";
    }

    private static DisplayValue DvTimeOnly(TimeOnly dt, PicClause pic)
    {
        return pic.Semantic switch
        {
            PicSemantic.Time6 => DisplayValue.FromNumber(dt.ToString("HHmmss"   , CultureInfo.InvariantCulture)),
            PicSemantic.Time9 => DisplayValue.FromNumber(dt.ToString("HHmmssfff", CultureInfo.InvariantCulture)),
            _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
        };
    }

    private static DisplayValue DvDateTime(DateTime dt, PicClause pic)
    {
        if (pic.Semantic != PicSemantic.Timestamp14)
        {
            throw new NotSupportedException($"DateTime can only be encoded as Timestamp14, but was {pic.Semantic}");
        }

        string text = dt.ToString("yyyyMMddHHmmss", CultureInfo.InvariantCulture);

        return DisplayValue.FromNumber(text);
    }
}