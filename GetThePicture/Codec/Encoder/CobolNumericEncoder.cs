using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolNumericEncoder
{
    public static string Encode(byte[] logicalBytes, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        // 根據PIC內容限制大小
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(logicalBytes, pic.TotalLength);

        byte[] cp950Bytes = Overpunch.Encode(fieldBytes, pic, options);

        Encoding cp950 = EncodingFactory.CP950;
        return cp950.GetString(cp950Bytes);
    }
}