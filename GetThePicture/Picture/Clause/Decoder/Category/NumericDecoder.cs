using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Decoder.Category;

public static class NumericDecoder
{
    /// <summary>
    /// CP950 → [Overpunch Decode]/[COMP] (object) → CLR value
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
            PicUsage.Display        => Display_Decode(fieldBytes, pic, options),
            PicUsage.PackedDecimal  =>   COMP3.Decode(fieldBytes, pic),
            PicUsage.Binary         =>   COMP4.Decode(fieldBytes, pic, options.Binary),
            PicUsage.NativeBinary   =>   COMP5.Decode(fieldBytes, pic, options.Binary),
            PicUsage.UPackedDecimal =>   COMP6.Decode(fieldBytes, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };
    }

    private static object Display_Decode(ReadOnlySpan<byte> fieldBytes, PicMeta pic, CodecOptions options)
    {
        var numeric = Base.Overpunch.OpCodec.Decode(fieldBytes, pic, options, out decimal sign);
        return ParseToValue(numeric, sign, pic);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="numeric">數字文</param>
    /// <param name="sign">(+/-)</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ParseToValue(byte[] numeric, decimal sign, PicMeta pic)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException($"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");

        if (numeric.Length != pic.DigitCount)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.DigitCount}, actual {numeric.Length}.");
        
        bool isNegative = sign < 0;

        decimal d = CbDecimal.Decode(numeric, pic.DecimalDigits, isNegative);

        // 有小數就直接返回 decimal，沒有小數返回最小 CLR 型別
        return (pic.DecimalDigits > 0) ? d : ConvertToClr(d, pic);
    }

    /// <summary>
    /// 整數的 CLR 型別轉換
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static object ConvertToClr(decimal value, PicMeta pic)
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
