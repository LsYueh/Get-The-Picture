using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class CobolPicDecoder
{
    /// <summary>
    /// COBOL PICTURE DISPLAY → CP950 → CLR value
    /// </summary>
    public static object Decode(string display, PicClause pic, CodecOptions codecOptions)
    {
        ArgumentNullException.ThrowIfNull(display);
        ArgumentNullException.ThrowIfNull(pic);

        Encoding cp950 = EncodingFactory.CP950;

        byte[] cp950Bytes = cp950.GetBytes(display);

        // 嚴格長度驗證（COBOL 是 fixed-length）
        if (codecOptions.Strict && (cp950Bytes.Length != pic.TotalLength))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.TotalLength}, actual {cp950Bytes.Length}.");
        }

#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.DataType)
        {
            case PicDataType.Numeric     : return      CobolNumericDecoder.Decode(cp950Bytes, pic, codecOptions);
            case PicDataType.Alphanumeric: return CobolAlphanumericDecoder.Decode(cp950Bytes, pic);
            case PicDataType.Alphabetic  : return   CobolAlphabeticDecoder.Decode(cp950Bytes, pic);
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type: {pic.DataType}");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }
}