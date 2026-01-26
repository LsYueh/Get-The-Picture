using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause.Decoder.Category;

internal static class AlphanumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC X.
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(ReadOnlySpan<byte> buffer, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        Encoding cp950 = Cobol.Utils.EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = Cobol.Utils.BufferSlice.SlicePadEnd(buffer, pic.DigitCount);
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
