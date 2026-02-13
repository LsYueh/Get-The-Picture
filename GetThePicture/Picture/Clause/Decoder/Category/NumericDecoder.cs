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
        Span<byte> bytes = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return pic.Usage switch
        {
            PicUsage.Display        => Display_Decode(bytes, pic, options),
            PicUsage.PackedDecimal  =>   COMP3.Decode(bytes, pic),
            PicUsage.Binary         =>   COMP4.Decode(bytes, pic, options.Binary),
            PicUsage.NativeBinary   =>   COMP5.Decode(bytes, pic, options.Binary),
            PicUsage.UPackedDecimal =>   COMP6.Decode(bytes, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };
    }

    private static object Display_Decode(Span<byte> bytes, PicMeta pic, CodecOptions options)
    {
        var numeric = Base.Overpunch.OpCodec.Decode(bytes, pic, options, out decimal sign);
        return ParseToValue(numeric, sign, pic);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="numeric">數字文</param>
    /// <param name="sign">(+/-)</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ParseToValue(Span<byte> numeric, decimal sign, PicMeta pic)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException($"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");

        if (numeric.Length != pic.DigitCount)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.DigitCount}, actual {numeric.Length}.");
        
        decimal value = CbDecimal.Decode(numeric, pic.DecimalDigits, sign < 0);

        return (pic.DecimalDigits == 0) ? Clr.AsInteger(value, pic) : value;
    }
}
