using System.Text;

using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.ComputationalBase;
using GetThePicture.Cobol.Picture.OverpunchBase;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder.Category;

internal static class NumericEncoder
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

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

        // Note: 要先變成DISPLAY的VALUE，再模擬DISPLAY時被S9(n)截位的輸出結果

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display       => Display_Encode(meta, pic, options),
            PicUsage.Binary        =>    COMP.Encode(meta, pic),
            PicUsage.PackedDecimal =>   COMP3.Encode(meta, pic),
            PicUsage.NativeBinary  =>   COMP5.Encode(meta, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        byte[] normalized = BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

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