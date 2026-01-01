using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Decoder;
using GetThePicture.Codec.Utils;

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

        if (pic.TotalLength > 28)
            throw new NotSupportedException($"PIC length {pic.TotalLength} exceeds the maximum supported length of 28.");

        Encoding cp950 = EncodingFactory.CP950;

        byte[] cp950Bytes = cp950.GetBytes(display);

        // 嚴格長度驗證（COBOL 是 fixed-length）
        if (strict && (cp950Bytes.Length != pic.TotalLength))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.TotalLength}, actual {cp950Bytes.Length}.");
        }

#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.DataType)
        {
            case PicDataType.Numeric     : return      CobolNumericDecoder.Decode(cp950Bytes, pic); // TODO: 想辦法塞 new CodecOptions().Sign... 
            case PicDataType.Alphanumeric: return CobolAlphanumericDecoder.Decode(cp950Bytes, pic);
            case PicDataType.Alphabetic  : return   CobolAlphabeticDecoder.Decode(cp950Bytes, pic);
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type: {pic.DataType}");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
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
