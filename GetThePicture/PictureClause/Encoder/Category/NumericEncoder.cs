using System.Text;

using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Options;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.ComputationalBase;
using GetThePicture.Cobol.Picture.OverpunchBase;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause.Encoder.Category;

internal static class NumericEncoder
{
    private static readonly Encoding cp950 = Cobol.Utils.EncodingFactory.CP950;

    /// <summary>
    /// Elementary Meta → Overpunch Encode → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="meta"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static byte[] Encode(ElementaryMeta meta, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display       => Display_Encode(meta, pic, options),
            PicUsage.Binary        =>    COMP.Encode(meta, pic, options.Binary),
            PicUsage.PackedDecimal =>   COMP3.Encode(meta, pic),
            PicUsage.NativeBinary  =>   COMP5.Encode(meta, pic, options.Binary),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        // Note: 模擬COBOL資料記憶體被S9(n)截位的輸出結果
        
        byte[] normalized = Cobol.Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return normalized;
    }

    private static byte[] Display_Encode(ElementaryMeta meta, PicClause pic, CodecOptions options)
    {
        string numeric = meta switch
        {
            { Type: EleType.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported meta type '{meta.Type}' for Numeric Text"),
        };

        decimal sign = meta.Sign;

        byte[] buffer = Overpunch.Encode(sign, numeric, pic, options);

        return buffer;
    }
}