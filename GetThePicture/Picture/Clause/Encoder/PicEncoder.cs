using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Encoder;

internal static class PicEncoder
{
    /// <summary>
    /// CLR value → [NumericValue] → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    /// <exception cref="FormatException"></exception>
    internal static byte[] Encode(object value, PicMeta pic, CodecOptions options)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        byte[] normalized = pic.Semantic switch
        {
            PicSemantic.GregorianDate or 
            PicSemantic.MinguoDate    =>      Semantic.DateEncoder.Encode(value, pic),
            PicSemantic.Time6 or 
            PicSemantic.Time9         =>      Semantic.TimeEncoder.Encode(value, pic),
            PicSemantic.Timestamp14   => Semantic.TimestampEncoder.Encode(value, pic),
            PicSemantic.Boolean       =>   Semantic.BooleanEncoder.Encode(value, pic),
            _ => EncodeBaseType(value, pic, options),
        };

        if (options.Strict && (normalized.Length != pic.StorageOccupied))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.StorageOccupied}, actual {normalized.Length}.");
        }

        return normalized;
    }

    private static byte[] EncodeBaseType(object value, PicMeta pic, CodecOptions options)
    {
        byte[] normalized;

        switch (pic.BaseClass)
        {
            case PicBaseClass.Numeric:
            {
                if (value is string text)
                    throw new NotSupportedException($"PIC {pic.Raw} expects Numeric value (number), but got string. Value: \"{text}\"");

                normalized = Category.NumericEncoder.Encode(value, pic, options);
                break;
            }

            case PicBaseClass.Alphanumeric:
            {
                if (value is not string text)
                    throw new NotSupportedException( $"PIC {pic.Raw} expects Alphanumeric value (string), but got {value?.GetType().Name ?? "null"}.");
                
                normalized = Category.AlphanumericEncoder.Encode(text, pic);
                break;
            }

            case PicBaseClass.Alphabetic:
            {
                if (value is not string text)
                    throw new NotSupportedException($"PIC {pic.Raw} expects Alphabetic value (string), but got {value?.GetType().Name ?? "null"}.");

                normalized = Category.AlphabeticEncoder.Encode(text, pic);
                break;
            }

            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseClass}");
        };

        return normalized;
    }
}
