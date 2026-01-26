using System.Globalization;

using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Options;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause.Encoder;

internal static class PicEncoder
{
    /// <summary>
    /// CLR value → Elementary Meta → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <param name="codecOptions"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    /// <exception cref="FormatException"></exception>
    public static byte[] Encode(object value, PicClause pic, CodecOptions codecOptions)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        // CLR value → Elementary Meta
        ElementaryMeta meta = ToElementaryMeta(value, pic);

        // Elementary Meta → COBOL Elementary Item (buffer)
        byte[] normalized = pic.BaseClass switch
        {
            PicBaseClass.Numeric      =>      Category.NumericEncoder.Encode(meta, pic, codecOptions),
            PicBaseClass.Alphanumeric => Category.AlphanumericEncoder.Encode(meta, pic),
            PicBaseClass.Alphabetic   =>   Category.AlphabeticEncoder.Encode(meta, pic),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseClass}"),
        };

        if (codecOptions.Strict && (normalized.Length != pic.StorageOccupied))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.StorageOccupied}, actual {normalized.Length}.");
        }

        return normalized;
    }

    /// <summary>
    /// CLR value → Elementary Meta (要跑Test)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    internal static ElementaryMeta ToElementaryMeta(object value, PicClause pic)
    {
        ElementaryMeta meta = value switch
        {
            string    s => EleString(s),
            char      c => EleString(c.ToString()),
            sbyte or byte or short or ushort or int or uint or long or ulong => EleInteger(value),
            float or double or decimal => EleDecimal(value, pic),
            DateOnly  d => EleDate(d, pic),
            TimeOnly  t => EleTime(t, pic),
            DateTime dt => EleTimeStamp(dt, pic),
            _ => throw new NotSupportedException($"Unsupported value type '{value.GetType()}'"),
        };

        return meta;
    }

    private static ElementaryMeta EleString(string text)
    {
        return ElementaryMeta.FromText(text);
    }

    private static ElementaryMeta EleInteger(object value)
    {
        bool isNegative;
        string digits;

        switch (value)
        {
            case sbyte v:
                isNegative = v < 0;
                digits = Math.Abs(v).ToString(CultureInfo.InvariantCulture);
                break;
            case short v:
                isNegative = v < 0;
                digits = Math.Abs(v).ToString(CultureInfo.InvariantCulture);
                break;
            case int v:
                isNegative = v < 0;
                digits = Math.Abs(v).ToString(CultureInfo.InvariantCulture);
                break;
            case long v:
                isNegative = v < 0;
                digits = Math.Abs(v).ToString(CultureInfo.InvariantCulture);
                break;
            case byte v:
                isNegative = false;
                digits = v.ToString(CultureInfo.InvariantCulture);
                break;
            case ushort v:
                isNegative = false;
                digits = v.ToString(CultureInfo.InvariantCulture);
                break;
            case uint v:
                isNegative = false;
                digits = v.ToString(CultureInfo.InvariantCulture);
                break;
            case ulong v:
                isNegative = false;
                digits = v.ToString(CultureInfo.InvariantCulture);
                break;
            default:
                throw new NotSupportedException($"Unsupported integer type '{value.GetType()}'");
        }

        return ElementaryMeta.FromNumber(isNegative, digits, decimalDigits: 0);
    }

    private static ElementaryMeta EleDecimal(object value, PicClause pic)
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

        return ElementaryMeta.FromNumber(isNegative, digits, scale);
    }

    private static decimal Pow10(int n)
    {
        decimal result = 1m;
        for (int i = 0; i < n; i++)
            result *= 10m;
        return result;
    }

    private static ElementaryMeta EleDate(DateOnly date, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Date' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");
            
        return pic.Semantic switch
        {
            PicSemantic.GregorianDate => ElementaryMeta.FromNumber(date.ToString("yyyyMMdd")),
            PicSemantic.MinguoDate => ElementaryMeta.FromNumber(ToMinguoDateString(date)),
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

    private static ElementaryMeta EleTime(TimeOnly dt, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Time' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        return pic.Semantic switch
        {
            PicSemantic.Time6 => ElementaryMeta.FromNumber(dt.ToString("HHmmss"   , CultureInfo.InvariantCulture)),
            PicSemantic.Time9 => ElementaryMeta.FromNumber(dt.ToString("HHmmssfff", CultureInfo.InvariantCulture)),
            _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
        };
    }

    private static ElementaryMeta EleTimeStamp(DateTime dt, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Timestamp' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");
            
        if (pic.Semantic != PicSemantic.Timestamp14)
        {
            throw new NotSupportedException($"DateTime can only be encoded as Timestamp14, but was {pic.Semantic}");
        }

        string text = dt.ToString("yyyyMMddHHmmss", CultureInfo.InvariantCulture);

        return ElementaryMeta.FromNumber(text);
    }
}