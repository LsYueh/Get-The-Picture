
using System.Globalization;

using GetThePicture.Cobol.Options;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.Items;

namespace GetThePicture.PictureClause.Decoder.Category;

internal static class NumericDecoder
{
    /// <summary>
    /// CP950 → Overpunch Decode  → CLR value
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        // Note: COBOL資料記憶體先被S9(n)截位再轉處裡，一般COBOL應該也是這樣的狀況

        // 截位或補字處理
        ReadOnlySpan<byte> fieldBytes = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return pic.Usage switch
        {
            PicUsage.Display       => Display_Decode(fieldBytes, pic, options),
            PicUsage.Binary        =>    Base.Computational.COMP.Decode(fieldBytes, pic, options.Binary),
            PicUsage.PackedDecimal =>   Base.Computational.COMP3.Decode(fieldBytes, pic),
            PicUsage.NativeBinary  =>   Base.Computational.COMP5.Decode(fieldBytes, pic, options.Binary),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };
    }

    private static object Display_Decode(ReadOnlySpan<byte> fieldBytes, PicMeta pic, CodecOptions options)
    {
        string numeric = Base.Overpunch.OpCodec.Decode(fieldBytes, pic, options, out decimal sign);
        return ParseToValue(numeric, sign, pic);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="numeric">數字文</param>
    /// <param name="sign">(+/-)</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ParseToValue(string numeric, decimal sign, PicMeta pic)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException( $"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");
        
        if (numeric.Length != pic.DigitCount)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.DigitCount}, actual {numeric.Length}.");

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
    private static object ConvertToClr(decimal value, PicMeta pic)
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
