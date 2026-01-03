using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolAlphabeticEncoder
{
    public static string Encode(string value, PicClause pic)
    {
        Encoding cp950 = EncodingFactory.CP950;

        byte[] cp950Bytes = cp950.GetBytes(value);

        // TODO: ...
        
        return cp950.GetString(cp950Bytes);
    }
}