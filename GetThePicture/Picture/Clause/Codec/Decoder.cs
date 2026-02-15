using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Codec;

public static class Decoder
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

        return pic.Semantic switch
        {
            PicSemantic.GregorianDate or
            PicSemantic.MinguoDate  =>      Semantic.Date.Decoder.Decode(buffer, pic),
            PicSemantic.Time6 or
            PicSemantic.Time9       =>      Semantic.Time.Decoder.Decode(buffer, pic),
            PicSemantic.Timestamp14 => Semantic.Timestamp.Decoder.Decode(buffer, pic),
            PicSemantic.Boolean     =>   Semantic.Boolean.Decoder.Decode(buffer, pic),
            _ => DecodeBaseType(buffer, pic, options),
        };
    }

    private static object DecodeBaseType(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions options)
    {
        return pic.BaseClass switch
        {
            PicBaseClass.Numeric      =>      Category.Numeric.Decoder.Decode(buffer, pic, options),
            PicBaseClass.Alphanumeric => Category.Alphanumeric.Decoder.Decode(buffer, pic),
            PicBaseClass.Alphabetic   =>   Category.Alphabetic.Decoder.Decode(buffer, pic),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type [Decode] : {pic.BaseClass}"),
        };
    }
}