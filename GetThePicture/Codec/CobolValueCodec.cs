using System.Text;

using GetThePicture.Codec.Decoder;
using GetThePicture.Cobol;

namespace GetThePicture.Codec;

public static class CobolValueCodec
{
    /// <summary>
    /// COBOL PICTURE DISPLAY → CLR value
    /// </summary>
    public static object Decode(string display, PicClause pic, bool strict = true)
    {
        ArgumentNullException.ThrowIfNull(display);
        ArgumentNullException.ThrowIfNull(pic);

        Encoding cp950 = EncodingFactory.CP950;

        byte[] cp950Bytes = cp950.GetBytes(display);

        // 嚴格長度驗證（COBOL 是 fixed-length）
        if (strict && (cp950Bytes.Length != pic.TotalLength))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.TotalLength}, actual {cp950Bytes.Length}.");
        }

        return pic.DataType switch
        {
            // PicDataType.Numeric      =>      CobolNumericDecoder.Decode(display, pic),
            PicDataType.Alphanumeric => CobolAlphanumericDecoder.Decode(cp950Bytes, pic),
            PicDataType.Alphabetic   =>   CobolAlphabeticDecoder.Decode(cp950Bytes, pic),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type: {pic.DataType}")
        };
    }

    /// <summary>
    /// CLR value → COBOL PICTURE DISPLAY
    /// </summary>
    public static string Encode(object value, PicClause pic)
    {
        // 之後實作
        throw new NotImplementedException("Encode is not implemented yet.");
    }
}
