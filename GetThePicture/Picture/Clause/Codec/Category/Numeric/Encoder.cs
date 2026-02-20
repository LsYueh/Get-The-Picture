using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Codec.Category.Numeric;

public static class Encoder
{
    /// <summary>
    /// CLR → {{ Meta → [Overpunch Encode]/[COMP] (byte) }} → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="nValuea"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="OverflowException"></exception>
    /// <exception cref="NotSupportedException"></exception>
    public static byte[] Encode(object value, PicMeta pic, CodecOptions? options = null)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException($"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");
        
        options ??= new CodecOptions();

        var nMeta = NumericMeta.Parse(value, pic);

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display => Display_Encode(nMeta, pic, options),
            PicUsage.COMP3   =>   COMP3.Encode(nMeta, pic),
            PicUsage.COMP4   =>   COMP4.Encode(nMeta, pic),
            PicUsage.COMP5   =>   COMP5.Encode(nMeta, pic, options.IsBigEndian),
            PicUsage.COMP6   =>   COMP6.Encode(nMeta, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        // Note: 模擬COBOL資料記憶體被S9(n)截位的輸出結果
        
        byte[] normalized = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return normalized;
    }

    private static byte[] Display_Encode(NumericMeta nMeta, PicMeta pic, CodecOptions options)
    {
        bool isNegative = nMeta.IsNegative;

        // Note: 如果不想複製，可直接用 ReadOnlyMemory<byte>
        byte[] numeric = [.. nMeta.Chars]; 

        byte[] buffer = Base.Overpunch.OpCodec.Encode(isNegative, numeric, pic, options);

        return buffer;
    }
}
