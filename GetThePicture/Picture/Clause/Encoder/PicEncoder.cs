using System.Globalization;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;
using static GetThePicture.Picture.Clause.Encoder.Category.NumericEncoder;

namespace GetThePicture.Picture.Clause.Encoder;

internal static class PicEncoder
{
    /// <summary>
    /// CLR value → COB Meta → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    /// <exception cref="FormatException"></exception>
    internal static byte[] Encode(object value, PicMeta pic, CodecOptions options)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        // object → COBOL Elementary Item (buffer)
        byte[] normalized = pic.Semantic switch
        {
            // PicSemantic.GregorianDate => // return Semantic.DateEncoder.Encode(buffer, pic);
            // PicSemantic.MinguoDate    => // return Semantic.DateEncoder.Encode(buffer, pic);
            // PicSemantic.Time6         => // return Semantic.TimeEncoder.Encode(buffer, pic);
            // PicSemantic.Time9         => // return Semantic.TimeEncoder.Encode(buffer, pic);
            // PicSemantic.Timestamp14   => // return Semantic.TimestampEncoder.Encode(buffer, pic);
            _ => EncodeBaseType(value, pic, options),
        };

        if (options.Strict && (normalized.Length != pic.StorageOccupied))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.StorageOccupied}, actual {normalized.Length}.");
        }

        return normalized;
    }

    private static byte[] EncodeBaseType(object value, PicMeta pic, CodecOptions options)
    {
        byte[] normalized;

        switch (pic.BaseClass)
        {
            case PicBaseClass.Numeric:
            {
                var nValue = EncodeNumeric(value, pic);
                normalized = Category.NumericEncoder.Encode(nValue, pic, options);
                break;
            }

            case PicBaseClass.Alphanumeric:
            {
                if (value is not string text)
                    throw new InvalidCastException( $"PIC {pic.Raw} expects Alphanumeric value (string), but got {value?.GetType().Name ?? "null"}.");
                
                normalized = Category.AlphanumericEncoder.Encode(text, pic);
                break;
            }

            case PicBaseClass.Alphabetic:
            {
                if (value is not string text)
                    throw new InvalidCastException($"PIC {pic.Raw} expects Alphabetic value (string), but got {value?.GetType().Name ?? "null"}.");

                normalized = Category.AlphabeticEncoder.Encode(text, pic);
                break;
            }

            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseClass}");
        };

        return normalized;
    }

    /// <summary>
    /// CLR value → NumericValue (要跑Test)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    internal static NumericValue EncodeNumeric(object value, PicMeta pic)
    {
        byte[] digits = new byte[pic.DigitCount];
        
        // 先轉 decimal
        decimal d = ToDecimal(value);
        
        bool isNegative = d < 0;
        if (isNegative)
            d = decimal.Negate(d); // 確保後面都是 magnitude

        // 無論小數點與位數，全部整數化後寫入 buffer
        decimal magnitude;

        // 有小數點或超過 18 位數 → 用 decimal
        if (pic.DecimalDigits > 0 || pic.DigitCount <= 18)
        {
            // 將小數點移動，變成整數
            decimal scale = (decimal)Math.Pow(10, pic.DecimalDigits);
            magnitude = d * scale;
        }
        else
        {
            magnitude = d;
        }

        // 根據位數使用 long 或 decimal 拆位寫入 buffer
        if (magnitude <= long.MaxValue)
        {
            WriteDigits((long)magnitude, digits);
        }
        else
        {
            WriteDigits(magnitude, digits);
        }

        return new NumericValue(isNegative, digits, pic.DecimalDigits);
    }

    private static decimal ToDecimal(object value) => value switch
    {
        byte b    => b,
        sbyte sb  => sb,
        short s   => s,
        ushort us => us,
        int i     => i,
        uint ui   => ui,
        long l    => l,
        ulong ul  => ul,
        float f   => (decimal)f,   // float → decimal
        double db => (decimal)db,  // double → decimal
        decimal d => d,
        _ => throw new NotSupportedException($"Unsupported numeric type: {value.GetType()}")
    };

    /// <summary>
    /// (long)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="buffer"></param>
    private static void WriteDigits(long value, Span<byte> buffer)
    {
        int i = buffer.Length - 1;

        // 從尾到頭寫入數字
        while (i >= 0)
        {
            buffer[i] = (byte)('0' + (value % 10));
            value /= 10;
            i--;
        }

        // 如果 value 已經用完，左側填 0
        while (i >= 0)
        {
            buffer[i] = (byte)'0';
            i--;
        }
    }

    /// <summary>
    /// (decimal)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="buffer"></param>
    private static void WriteDigits(decimal value, Span<byte> buffer)
    {
        int i = buffer.Length - 1;

        // 從尾到頭寫入數字
        while (i >= 0)
        {
            decimal rem = value % 10;
            buffer[i] = (byte)('0' + (int)rem);
            value = decimal.Truncate(value / 10);
            i--;
        }

        // 左側填 0
        while (i >= 0)
        {
            buffer[i] = (byte)'0';
            i--;
        }
    }

    // private static CobMeta EleDate(DateOnly date, PicMeta pic)
    // {
    //     if (pic.Usage != PicUsage.Display)
    //         throw new NotSupportedException($"'Date' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");
            
    //     return pic.Semantic switch
    //     {
    //         PicSemantic.GregorianDate => CobMeta.FromNumber(date.ToString("yyyyMMdd")),
    //         PicSemantic.MinguoDate => CobMeta.FromNumber(ToMinguoDateString(date)),
    //         _ => throw new NotSupportedException($"Unsupported DateOnly format: {pic.Semantic}")
    //     };
    // }

    // private static string ToMinguoDateString(DateOnly date)
    // {
    //     int rocYear = date.Year - 1911;
        
    //     if (rocYear <= 0)
    //     {
    //         throw new ArgumentOutOfRangeException(nameof(date), "Date is before ROC calendar starts (1912-01-01).");
    //     }

    //     return $"{rocYear:000}{date:MMdd}";
    // }

    // private static CobMeta EleTime(TimeOnly dt, PicMeta pic)
    // {
    //     if (pic.Usage != PicUsage.Display)
    //         throw new NotSupportedException($"'Time' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

    //     return pic.Semantic switch
    //     {
    //         PicSemantic.Time6 => CobMeta.FromNumber(dt.ToString("HHmmss"   , CultureInfo.InvariantCulture)),
    //         PicSemantic.Time9 => CobMeta.FromNumber(dt.ToString("HHmmssfff", CultureInfo.InvariantCulture)),
    //         _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
    //     };
    // }

    // private static CobMeta EleTimeStamp(DateTime dt, PicMeta pic)
    // {
    //     if (pic.Usage != PicUsage.Display)
    //         throw new NotSupportedException($"'Timestamp' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");
            
    //     if (pic.Semantic != PicSemantic.Timestamp14)
    //     {
    //         throw new NotSupportedException($"DateTime can only be encoded as Timestamp14, but was {pic.Semantic}");
    //     }

    //     string text = dt.ToString("yyyyMMddHHmmss", CultureInfo.InvariantCulture);

    //     return CobMeta.FromNumber(text);
    // }
}