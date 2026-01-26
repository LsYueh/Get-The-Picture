using System.Text;

using GetThePicture.Cobol.Meta;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.Items;

namespace GetThePicture.PictureClause.Encoder.Category;

internal static class AlphanumericEncoder
{
    private static readonly Encoding cp950 = Utils.EncodingFactory.CP950;

    public static byte[] Encode(CobMeta meta, PicMeta pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        var text = meta switch
        {
            { Type: CobMetaType.Text,   Text:   { } t } => t.Value,
            { Type: CobMetaType.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported meta type '{meta.Type}' for Alphanumeric Text"),
        };

        byte[] buffer = cp950.GetBytes(text);

        byte[] normalized = Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

        return normalized;
    }
}
