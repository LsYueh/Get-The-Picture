using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class CobolAlphanumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC X.
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static string Decode(byte[] cp950Bytes, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(cp950Bytes, pic.TotalLength);
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
