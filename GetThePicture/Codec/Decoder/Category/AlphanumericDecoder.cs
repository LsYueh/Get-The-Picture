using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder.Category;

internal static class AlphanumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC X.
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(byte[] cp950Bytes, PicClause pic)
    {
        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"PIC X does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        Encoding cp950 = EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(cp950Bytes, pic.DigitCount);
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
