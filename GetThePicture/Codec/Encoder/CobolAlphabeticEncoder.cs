using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolAlphabeticEncoder
{
    public static string Encode(byte[] cp950Bytes, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(cp950Bytes, pic.TotalLength);

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