using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Decoder;

public static class PicDecoder
{
    /// <summary>
    /// COBOL Elementary Item (buffer) → CLR value
    /// </summary>
    /// <param name="buffer">COBOL Elementary Item</param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions options)
    {
        ArgumentNullException.ThrowIfNull(pic);

        if (options.Strict && (buffer.Length != pic.StorageOccupied))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.StorageOccupied}, actual {buffer.Length}.");
        }

#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.Semantic)
        {
            case PicSemantic.GregorianDate : 
            case PicSemantic.MinguoDate    : return      Semantic.DateDecoder.Decode(buffer, pic);
            case PicSemantic.Time6         : 
            case PicSemantic.Time9         : return      Semantic.TimeDecoder.Decode(buffer, pic);
            case PicSemantic.Timestamp14   : return Semantic.TimestampDecoder.Decode(buffer, pic);
            case PicSemantic.Boolean       : return   Semantic.BooleanDecoder.Decode(buffer, pic);
            default:
                return DecodeBaseType(buffer, pic, options);
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }

    private static object DecodeBaseType(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions options)
    {
#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (pic.BaseClass)
        {
            case PicBaseClass.Numeric     : return      Category.NumericDecoder.Decode(buffer, pic, options);
            case PicBaseClass.Alphanumeric: return Category.AlphanumericDecoder.Decode(buffer, pic);
            case PicBaseClass.Alphabetic  : return   Category.AlphabeticDecoder.Decode(buffer, pic);
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Decode] : {pic.BaseClass}");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }
}