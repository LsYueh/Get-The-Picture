using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolAlphanumericEncoder
{
    public static string Encode(byte[] cp950Bytes, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        // X(n) 通常右補空白
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadEnd(cp950Bytes, pic.TotalLength);

        return cp950.GetString(fieldBytes);
    }
}
