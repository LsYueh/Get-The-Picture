using System.Text;

using GetThePicture.Picture.Base.Clause.Items;
using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Picture.Codec.Category.Alphabetic;

internal static class Encoder
{
    private static readonly Encoding cp950 = Utils.EncodingFactory.CP950;

    public static byte[] Encode(string text, PicMeta pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC A does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        byte[] buffer = cp950.GetBytes(text);

        byte[] normalized = Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

        // PIC A 檢查
        for (int i = 0; i < normalized.Length; i++)
        {
            byte b = normalized[i];

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
        
        return normalized;
    }
}