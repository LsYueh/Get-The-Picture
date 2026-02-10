using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Encoder.Category;

public static class NumericEncoder
{
    public readonly struct NumericValue(bool isNegative, byte[] magnitude, int decimalDigits)
    {
        public bool IsNegative { get; } = isNegative;

        /// <summary>
        /// 純數字，不含符號
        /// </summary>
        public ReadOnlyMemory<byte> Magnitude { get; } = magnitude;
        
        public int DecimalDigits { get; } = decimalDigits;

        /// <summary>
        /// 原始數值，方便計算
        /// </summary>
        public decimal Value { get; } = ParseDecimal(isNegative, magnitude, decimalDigits);

        private static decimal ParseDecimal(bool isNegative, ReadOnlySpan<byte> magnitude, int decimalDigits)
        {
            if (magnitude.Length == 0)
                return 0m;

            decimal result = 0m;
            foreach (byte b in magnitude)
            {
                int digit = b - (byte)'0';
                if (digit < 0 || digit > 9)
                    throw new FormatException($"Invalid numeric digit: {(char)b}");
                result = result * 10 + digit;
            }

            if (decimalDigits > 0)
                result /= (decimal)Math.Pow(10, decimalDigits);

            if (isNegative)
                result = -result;

            return result;
        }

    }

    /// <summary>
    /// Meta → Overpunch Encode → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="nValuea"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static byte[] Encode(NumericValue nValue, PicMeta pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display       =>                  Display_Encode(nValue, pic, options),
            PicUsage.Binary        =>  Base.Computational.COMP.Encode(nValue, pic, options.Binary),
            PicUsage.PackedDecimal => Base.Computational.COMP3.Encode(nValue, pic),
            PicUsage.NativeBinary  => Base.Computational.COMP5.Encode(nValue, pic, options.Binary),
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
        byte[] numeric = nValue.Magnitude.ToArray(); // 如果不想複製，可直接用 ReadOnlyMemory<byte>

        byte[] buffer = Base.Overpunch.OpCodec.Encode(sign, numeric, pic, options);

        return buffer;
    }
}