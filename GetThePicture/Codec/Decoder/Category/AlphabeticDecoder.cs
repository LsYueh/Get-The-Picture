using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder.Category;

internal static class AlphabeticDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC A.
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(byte[] cp950Bytes, PicClause pic)
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
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
