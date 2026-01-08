using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolNumericEncoder
{
    public static string Encode(byte[] logicalBytes, PicClause pic, CodecOptions? options = null)
    {
        Encoding cp950 = EncodingFactory.CP950;

        options ??= new CodecOptions();

        byte[] cp950Bytes = Overpunch.Encode(logicalBytes, pic, options);

        // 根據PIC內容限制大小
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(cp950Bytes, pic.TotalLength);

        return cp950.GetString(fieldBytes);
    }
}