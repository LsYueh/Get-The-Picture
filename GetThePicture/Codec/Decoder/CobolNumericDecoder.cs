
using System.Globalization;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class CobolNumericDecoder
{
    /// <summary>
    /// CP950 → Overpunch Decode  → CLR value
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(byte[] cp950Bytes, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        // Note: VALUE進來後可能在DISPLAY被S9(n)截位，再轉輸出結果，一般COBOL應該也是這樣的狀況

        // 截位或補字處理
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(cp950Bytes, pic.TotalLength);

        // 轉換
        string numeric = Overpunch.Decode(fieldBytes, pic, options, out decimal sign);
        
        // 輸出
        return ParseToValue(numeric, sign, pic);
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
