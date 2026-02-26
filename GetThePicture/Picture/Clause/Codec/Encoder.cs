using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Codec.Semantic;

namespace GetThePicture.Picture.Clause.Codec;

public static class Encoder
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
    public static byte[] Encode(object value, PicMeta pic, CodecOptions options)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        if (pic.Semantic != PicSemantic.None)
        {
            Constraint rule = Rules.GetConstraint(pic.Semantic);
            rule.ValidateOrThrow(pic);
        }

        byte[] normalized = pic.Semantic switch
        {
            PicSemantic.GregorianDate or 
            PicSemantic.MinguoDate    =>      Semantic.Date.Encoder.Encode(value, pic),
            PicSemantic.Time6 or 
            PicSemantic.Time9         =>      Semantic.Time.Encoder.Encode(value, pic),
            PicSemantic.Timestamp14   => Semantic.Timestamp.Encoder.Encode(value, pic),
            PicSemantic.Boolean       =>   Semantic.Boolean.Encoder.Encode(value, pic),
            _ => EncodeBaseType(value, pic, options),
        };

        if (options.Strict && (normalized.Length != pic.StorageOccupied))
        {
            throw new FormatException($"DISPLAY length mismatch. Expected {pic.StorageOccupied}, actual {normalized.Length}.");
        }

        return normalized;
    }

    internal static byte[] EncodeBaseType(object value, PicMeta pic, CodecOptions options)
    {
        byte[] normalized;

        switch (pic.BaseClass)
        {
            case PicBaseClass.Numeric:
            {
                if (value is string text)
                    throw new NotSupportedException($"PIC {pic.Raw} expects Numeric value (number), but got string. Value: \"{text}\"");

                normalized = Category.Numeric.Encoder.Encode(value, pic, options);
                break;
            }

            case PicBaseClass.Alphanumeric:
            {
                if (value is not string text)
                    throw new NotSupportedException( $"PIC {pic.Raw} expects Alphanumeric value (string), but got {value?.GetType().Name ?? "null"}.");
                
                normalized = Category.Alphanumeric.Encoder.Encode(text, pic);
                break;
            }

            case PicBaseClass.Alphabetic:
            {
                if (value is not string text)
                    throw new NotSupportedException($"PIC {pic.Raw} expects Alphabetic value (string), but got {value?.GetType().Name ?? "null"}.");

                normalized = Category.Alphabetic.Encoder.Encode(text, pic);
                break;
            }

            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseClass}");
        };

        return normalized;
    }
}
