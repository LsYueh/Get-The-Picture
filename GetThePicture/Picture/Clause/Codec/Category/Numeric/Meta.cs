using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.Computational.Base;

namespace GetThePicture.Picture.Clause.Codec.Category.Numeric;

public readonly struct NumericMeta(byte[] chars, int decimalDigits, bool isNegative)
{
    /// <summary>
    /// 純數字，不含符號
    /// </summary>
    public byte[] Chars { get; } = chars;

    public int DecimalDigits { get; } = decimalDigits;

    public bool IsNegative { get; } = isNegative;

    /// <summary>
    /// 原始數值，方便計算
    /// </summary>
    public decimal Value { get; } = CbDecimal.Decode(chars, decimalDigits, isNegative);

    public long ToInt64()
    {
        if (DecimalDigits != 0)
            throw new InvalidOperationException("Cannot convert to Int64 when decimal digits exist.");

        if (Chars.Length == 0)
            throw new FormatException("Empty numeric value.");

        long result = 0;

        if (IsNegative)
        {
            foreach (byte b in Chars)
            {
                int digit = b - (byte)'0';
                result = checked(result * 10 - digit);
            }
        }
        else
        {
            foreach (byte b in Chars)
            {
                int digit = b - (byte)'0';
                result = checked(result * 10 + digit);
            }
        }

        return result;
    }

    public ulong ToUInt64()
    {
        if (DecimalDigits != 0)
            throw new InvalidOperationException("Cannot convert to UInt64 when decimal digits exist.");

        if (Chars.Length == 0)
            throw new FormatException("Empty numeric value.");

        if (IsNegative)
            throw new OverflowException("Negative value cannot convert to UInt64.");

        ulong result = 0;

        foreach (byte b in Chars)
        {
            result = checked(result * 10 + (ulong)(b - (byte)'0'));
        }

        return result;
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    public static NumericMeta Parse(object value, PicMeta pic)
    {
        if (!IsNumericType(value))
            throw new NotSupportedException($"Type {value.GetType().Name} is not supported.");

        byte[] digits = new byte[pic.DigitCount];

        bool isNegative;

        // 特定格式分開處裡
        if (pic.DecimalDigits == 0 && pic.DigitCount <= 18)
        {
            isNegative = EncodeInt64(value, pic, digits);
        }
        else
        {
            isNegative = EncodeDecimal(value, pic, digits);
        }

        return new NumericMeta(digits, pic.DecimalDigits, isNegative);
    }

    private static bool EncodeInt64(object value, PicMeta pic, Span<byte> buffer)
    {
        long v = Convert.ToInt64(value);
        bool isNegative = v < 0;

        ulong absValue;

        if (isNegative)
        {
            absValue = v == long.MinValue ? (ulong)long.MaxValue + 1 : (ulong)(-v);
        }
        else
        {
            absValue = (ulong)v;
        }

        FillDigitsUInt64(absValue, buffer);

        return isNegative;
    }

    private static bool EncodeDecimal(object value, PicMeta pic, Span<byte> buffer)
    {
        // 先轉 decimal
        decimal d = ToDecimal(value);

        // 符號處理
        bool isNegative = d < 0;
        if (isNegative)
            d = decimal.Negate(d); // 確保後面都是 magnitude

        // scale 小數點
        decimal magnitude = (pic.DecimalDigits > 0)
            ? d * CbDecimal.Pow10(pic.DecimalDigits) : d;

        // 拆位填 buffer
        if (magnitude <= ulong.MaxValue)
        {
            FillDigitsUInt64((ulong)magnitude, buffer);
        }
        else
        {
            FillDigitsDecimal(magnitude, buffer);
        }

        return isNegative;
    }

    private static void FillDigitsUInt64(ulong value, Span<byte> buffer)
    {
        int i = buffer.Length - 1;

        // 從尾到頭寫入數字
        // COBOL 特性：超過 buffer 位數的高位會被截斷 (silent truncation)
        while (i >= 0 && value > 0)
        {
            buffer[i] = (byte)('0' + (value % 10));
            value /= 10;
            i--;
        }

        // 左側填 0
        while (i >= 0)
        {
            buffer[i] = (byte)'0';
            i--;
        }

        // Note: 如果 value > 0，表示超過 buffer 容量，高位會被丟掉
    }

    private static void FillDigitsDecimal(decimal value, Span<byte> buffer)
    {
        int i = buffer.Length - 1;

        // 從尾到頭寫入數字
        // COBOL 特性：超過 buffer 位數的高位會被截斷 (silent truncation)
        while (i >= 0 && value > 0)
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

        // Note: 如果 value > 0，表示超過 buffer 容量，高位會被丟掉
    }

    private static decimal ToDecimal(object value) => value switch
    {
        byte b => b,
        sbyte sb => sb,
        short s => s,
        ushort us => us,
        int i => i,
        uint ui => ui,
        long l => l,
        ulong ul => ul,
        float f => (decimal)f,   // float → decimal
        double db => (decimal)db,  // double → decimal
        decimal d => d,
        _ => throw new NotSupportedException($"Unsupported numeric type: {value.GetType()}")
    };

    private static bool IsNumericType(object value)
    {
        return value is byte || value is sbyte ||
            value is short || value is ushort ||
            value is int || value is uint ||
            value is long || value is ulong ||
            value is decimal || value is float || value is double;
    }
}
