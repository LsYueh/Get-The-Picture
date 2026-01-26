using System.Text;

using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.Items;

namespace GetThePicture.PictureClause.Decoder.Category;

internal static class AlphanumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC X.
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        Encoding cp950 = Utils.EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
