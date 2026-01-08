using System.Text;

using GetThePicture.Cobol.Picture;
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
        switch (pic.Semantic)
        {
            // TODO: 根據PicClause來處理cp950Bytes...
            case PicSemantic.GregorianDate : 
            case PicSemantic.MinguoDate    : 
            case PicSemantic.Time6         : 
            case PicSemantic.Time9         : 
            case PicSemantic.Timestamp14   : 
            default:
                return DecodeBaseType(cp950Bytes, pic, codecOptions);
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }

    private static object DecodeBaseType(byte[] cp950Bytes, PicClause pic, CodecOptions codecOptions)
    {
#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.BaseType)
        {
            case PicBaseType.Numeric     : return      CobolNumericDecoder.Decode(cp950Bytes, pic, codecOptions);
            case PicBaseType.Alphanumeric: return CobolAlphanumericDecoder.Decode(cp950Bytes, pic);
            case PicBaseType.Alphabetic  : return   CobolAlphabeticDecoder.Decode(cp950Bytes, pic);
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Decode] : {pic.BaseType}");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }
}