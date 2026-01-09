using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolNumericEncoder
{
    public static string Encode(DisplayValue displayValue, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Numeric Text"),
        };

        byte[] buffer = cp950.GetBytes(text);

        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(buffer, pic.TotalLength);

        // TODO: sign + numeric 要傳入 Overpunch.Encode()...

        string numeric = cp950.GetString(Overpunch.Encode(fieldBytes, pic, options));

        return numeric;
    }
}