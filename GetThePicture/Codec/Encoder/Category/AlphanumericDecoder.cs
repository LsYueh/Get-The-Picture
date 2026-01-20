using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder.Category;

internal static class AlphanumericEncoder
{
    public static string Encode(DisplayValue displayValue, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");
            
        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Text,   Text:   { } t } => t.Value,
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Alphanumeric Text"),
        };

        byte[] buffer = cp950.GetBytes(text);

        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

        return cp950.GetString(fieldBytes);
    }
}
