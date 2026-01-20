using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Decoder.Category;
using GetThePicture.Codec.Decoder.Semantic;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class PicDecoder
{
    /// <summary>
    /// 
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <param name="codecOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(ReadOnlySpan<byte> buffer, PicClause pic, CodecOptions codecOptions)
    {
        ArgumentNullException.ThrowIfNull(pic);

#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.Semantic)
        {
            // case PicSemantic.GregorianDate : 
            // case PicSemantic.MinguoDate    : return DateDecoder.Decode(display, pic);
            // case PicSemantic.Time6         : 
            // case PicSemantic.Time9         : return TimeDecoder.Decode(display, pic);
            // case PicSemantic.Timestamp14   : return TimestampDecoder.Decode(display, pic);
            default:
                return DecodeBaseType(buffer, pic, codecOptions);
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }

    private static object DecodeBaseType(ReadOnlySpan<byte> buffer, PicClause pic, CodecOptions codecOptions)
    {
#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.BaseClass)
        {
            // case PicBaseClass.Numeric     : return      NumericDecoder.Decode(buffer, pic, codecOptions);
            // case PicBaseClass.Alphanumeric: return AlphanumericDecoder.Decode(buffer, pic);
            case PicBaseClass.Alphabetic  : return   AlphabeticDecoder.Decode(buffer, pic);
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Decode] : {pic.BaseClass}");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }
}