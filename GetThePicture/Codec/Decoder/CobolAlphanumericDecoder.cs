using System.Text;
using GetThePicture.Cobol;

namespace GetThePicture.Codec.Decoder;

internal static class CobolAlphanumericDecoder
{
    public static string Decode(byte[] cp950Bytes, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;
        
        string s = cp950.GetString(cp950Bytes);

        // X(n) DISPLAY 通常右補空白
        return s.TrimEnd();
    }
}
