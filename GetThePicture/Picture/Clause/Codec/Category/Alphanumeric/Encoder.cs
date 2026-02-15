using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Category.Alphanumeric;

internal static class Encoder
{
    private static readonly Encoding cp950 = Utils.EncodingFactory.CP950;

    public static byte[] Encode(string text, PicMeta pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        byte[] buffer = cp950.GetBytes(text);

        byte[] normalized = Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);

        return normalized;
    }
}
