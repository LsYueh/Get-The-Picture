using System.Text;
using GetThePicture.Cobol;

namespace GetThePicture.Codec.Decoder;

internal static class CobolAlphanumericDecoder
{
    public static string Decode(byte[] cp950Bytes, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(cp950Bytes, 0, pic.TotalLength);
        
        string value = cp950.GetString(fieldBytes);

        return value.TrimEnd();
    }
}
