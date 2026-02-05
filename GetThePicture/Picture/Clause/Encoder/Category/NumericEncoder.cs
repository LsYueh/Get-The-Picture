using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Encoder.Meta;

namespace GetThePicture.Picture.Clause.Encoder.Category;

internal static class NumericEncoder
{
    /// <summary>
    /// Meta → Overpunch Encode → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="meta"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static byte[] Encode(CobMeta meta, PicMeta pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display       =>                  Display_Encode(meta, pic, options),
            PicUsage.Binary        =>  Base.Computational.COMP.Encode(meta, pic, options.Binary),
            PicUsage.PackedDecimal => Base.Computational.COMP3.Encode(meta, pic),
            PicUsage.NativeBinary  => Base.Computational.COMP5.Encode(meta, pic, options.Binary),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        // Note: 模擬COBOL資料記憶體被S9(n)截位的輸出結果
        
        byte[] normalized = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return normalized;
    }

    private static byte[] Display_Encode(CobMeta meta, PicMeta pic, CodecOptions options)
    {
        string numeric = meta switch
        {
            { Type: CobMetaType.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported meta type '{meta.Type}' for Numeric Text"),
        };

        decimal sign = meta.Sign;

        byte[] buffer = Base.Overpunch.OpCodec.Encode(sign, numeric, pic, options);

        return buffer;
    }
}