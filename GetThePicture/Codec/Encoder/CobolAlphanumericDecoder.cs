using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolAlphanumericEncoder
{
    public static string Encode(DisplayValue displayValue, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Text,   Text:   { } t } => t.Value,
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Alphanumeric Text"),
        };

        byte[] buffer = cp950.GetBytes(text);

        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(buffer, pic.TotalLength);

        return cp950.GetString(fieldBytes);
    }
}
