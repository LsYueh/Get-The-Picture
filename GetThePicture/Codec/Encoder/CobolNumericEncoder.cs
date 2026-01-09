using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolNumericEncoder
{
    /// <summary>
    /// Display Value → Overpunch Encode → COBOL PICTURE DISPLAY
    /// </summary>
    /// <param name="displayValue"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static string Encode(DisplayValue displayValue, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Numeric Text"),
        };

        // TODO: DisplayValue (sign + numeric) 要傳入 Overpunch.Encode()...
        byte[] buffer = Overpunch.Encode(cp950.GetBytes(text), pic, options);

        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(buffer, pic.TotalLength);

        return cp950.GetString(fieldBytes);
    }
}