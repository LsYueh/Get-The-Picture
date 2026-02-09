using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Decoder.Category;

internal static class AlphabeticDecoder
{
    private static readonly Encoding cp950 = Utils.EncodingFactory.CP950;
    
    /// <summary>
    /// Decoder for COBOL PIC A.
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC A does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

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
