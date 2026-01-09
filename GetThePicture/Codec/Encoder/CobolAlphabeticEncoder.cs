using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolAlphabeticEncoder
{
    public static string Encode(DisplayValue displayValue, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        var text = displayValue switch
        {
            { Kind: DisplayValueKind.Text,   Text:   { } t } => t.Value,
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}'"),
        };

        byte[] buffer = cp950.GetBytes(text);

        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(buffer, pic.TotalLength);

        // PIC A 檢查
        for (int i = 0; i < fieldBytes.Length; i++)
        {
            byte b = fieldBytes[i];

            // space
            if (b == 0x20)
                continue;

            // A-Z
            if (b >= 0x41 && b <= 0x5A)
                continue;

            // a-z
            if (b >= 0x61 && b <= 0x7A)
                continue;

            throw new FormatException($"PIC A : Invalid byte 0x{b:X2} at position {i+1}"); // Note: 轉成 1-based
        }
        
        return cp950.GetString(fieldBytes);
    }
}