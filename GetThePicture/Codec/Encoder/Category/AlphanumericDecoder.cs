using System.Text;

using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder.Category;

internal static class AlphanumericEncoder
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    public static byte[] Encode(ElementaryMeta meta, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        var text = meta switch
        {
            { Type: EleType.Text,   Text:   { } t } => t.Value,
            { Type: EleType.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported meta type '{meta.Type}' for Alphanumeric Text"),
        };

        byte[] buffer = cp950.GetBytes(text);

        byte[] normalized = BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

        return normalized;
    }
}
