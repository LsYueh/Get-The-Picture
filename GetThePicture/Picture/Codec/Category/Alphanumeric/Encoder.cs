using System.Text;

using GetThePicture.Picture.Base.Clause.Items;
using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Picture.Codec.Category.Alphanumeric;

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
