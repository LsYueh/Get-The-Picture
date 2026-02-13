using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Encoder.Category;

public static class NumericEncoder
{
    /// <summary>
    /// Meta → [Overpunch Encode]/[COMP] (byte) → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="nValuea"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static byte[] Encode(object value, PicMeta pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        var nValue = EncodeNumeric(value, pic);

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display        => Display_Encode(nValue, pic, options),
            PicUsage.PackedDecimal  =>   COMP3.Encode(nValue, pic),
            PicUsage.Binary         =>   COMP4.Encode(nValue, pic, options.Binary),
            PicUsage.NativeBinary   =>   COMP5.Encode(nValue, pic, options.Binary),
            PicUsage.UPackedDecimal =>   COMP6.Encode(nValue, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        // Note: 模擬COBOL資料記憶體被S9(n)截位的輸出結果
        
        byte[] normalized = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return normalized;
    }

    private static byte[] Display_Encode(NumericValue nValue, PicMeta pic, CodecOptions options)
    {
        // sign: 1 或 -1
        decimal sign = nValue.IsNegative ? -1.0m : 1.0m;

        // magnitude buffer
        byte[] numeric = nValue.Chars.ToArray(); // 如果不想複製，可直接用 ReadOnlyMemory<byte>

        byte[] buffer = Base.Overpunch.OpCodec.Encode(sign, numeric, pic, options);

        return buffer;
    }

    // Numeric Value Meta

    public readonly struct NumericValue(byte[] chars, int decimalDigits, bool isNegative)
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

        return new NumericValue(digits, pic.DecimalDigits, isNegative);
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
    }
}