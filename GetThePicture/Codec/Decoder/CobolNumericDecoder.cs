using System.Text;
using System.Globalization;

using GetThePicture.Cobol;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class CobolNumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC 9/S9.
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(byte[] cp950Bytes, PicClause pic, CodecOptions? options = null)
    {
        Encoding cp950 = EncodingFactory.CP950;

        options ??= new CodecOptions();

        // 根據PIC內容限制大小
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(cp950Bytes, 0, pic.TotalLength);

        // 解析出符號(sign)與數字文(numeric)
        Span<byte> span = OverpunchDecode(fieldBytes, pic, options, out decimal sign);
        EnsureAllAsciiDigits(span);
        string numeric = cp950.GetString(span);
        
        // 轉換成數字
        return ParseToValue(numeric, sign, pic);
    }

    private static byte[] OverpunchDecode(ReadOnlySpan<byte> fieldBytes, PicClause pic, CodecOptions options, out decimal sign)
    {
        byte[] buffer = new byte[fieldBytes.Length];
        fieldBytes.CopyTo(buffer);

        sign = 1.0m;

        if (pic.Signed)
        {
            OpVal opVal = GetOverpunchValue(fieldBytes, options);

            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            buffer[index] = (byte) opVal.Digit;
            sign = opVal.Sign;
        }

        return buffer;
    }

    private static OpVal GetOverpunchValue(ReadOnlySpan<byte> fieldBytes, CodecOptions options)
    {
        if (fieldBytes.IsEmpty)
            throw new FormatException("Field bytes is empty.");

        if (!OverpunchCode.Map.TryGetValue(options.DataStorage, out Dictionary<char, OpVal>? codex))
            throw new FormatException($"Unsupported DataStorage: {options.DataStorage}");

        Index index = options.Sign switch
        {
            SignOptions.IsTrailing => ^1,
            SignOptions.IsLeading  => 0,
            _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
        };

        char key = (char)(fieldBytes[index] & 0x7F); // ASCII overpunch

        if (!codex.TryGetValue(key, out OpVal value))
            throw new FormatException($"Invalid overpunch digit: '{key}'");

        return value;
    }

    private static void EnsureAllAsciiDigits(ReadOnlySpan<byte> span)
    {
        for (int i = 0; i < span.Length; i++)
        {
            if ((uint)(span[i] - (byte)'0') > 9)
                throw new FormatException($"Invalid digit at index {i+1}"); // Note: 轉成 1-based
        }
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="numeric">數字文</param>
    /// <param name="sign">(+/-)</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ParseToValue(string numeric, decimal sign, PicClause pic)
    {
        if (pic.TotalLength > 28)
            throw new OverflowException( $"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.TotalLength} digit(s), which exceeds the supported maximum (28 digits).");
        
        if (numeric.Length != pic.TotalLength)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.TotalLength}, actual {numeric.Length}.");

        // 插入小數點
        string withDot = (pic.DecimalDigits > 0) ? numeric.Insert(pic.IntegerDigits, ".") : numeric;

        // 統一轉成decimal後再處理型別
        if (!decimal.TryParse(withDot, NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture, out decimal numbers))
        {
            throw new FormatException($"Invalid numeric value: '{withDot} ({numeric})'");
        }

        // 帶入正負號
        decimal value = sign * numbers;

        // 輸出
        return (pic.DecimalDigits != 0) ? value : ConvertToClr(value, pic);
    }

    /// <summary>
    /// 整數的 CLR 型別轉換
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ConvertToClr(decimal value, PicClause pic)
    {
        int totalDigits = pic.IntegerDigits;
        bool signed = pic.Signed;

        // 根據 PIC 和 value 決定最佳型別
        
        if (totalDigits <= 2)
        {
            if (!signed) return (byte)value;
            return value >= 0 ? (byte)value : (sbyte)value; // fallback
        }

        if (totalDigits <= 4)
        {
            if (!signed) return (ushort)value;
            return value >= 0 ? (ushort)value : (short)value; // fallback
        }

        if (totalDigits <= 9)
        {
            if (!signed) return (uint)value;
            return value >= 0 ? (uint)value : (int)value; // fallback
        }

        if (totalDigits <= 18)
        {
            if (!signed) return (ulong)value;
            return value >= 0 ? (ulong)value : (long)value; // fallback
        }

        // 超過 18 位數，一律用 decimal
        return value;
    }

}
