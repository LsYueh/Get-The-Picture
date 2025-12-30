using GetThePicture.Codec.Decoder;
using GetThePicture.Pic;

namespace GetThePicture.Codec;

public static class CobolValueCodec
{
    /// <summary>
    /// DISPLAY → CLR value
    /// </summary>
    public static object Decode(string display, PicClause pic)
    {
        ArgumentNullException.ThrowIfNull(display);
        ArgumentNullException.ThrowIfNull(pic);

        // 嚴格長度驗證（COBOL 是 fixed-length）
        if (display.Length != pic.TotalLength)
        {
            throw new FormatException(
                $"DISPLAY length mismatch. Expected {pic.TotalLength}, actual {display.Length}.");
        }

        return pic.DataType switch
        {
            PicDataType.Alphanumeric => CobolAlphanumericDecoder.Decode(display, pic),
            // PicDataType.Alphabetic   => CobolAlphanumericDecoder.Decode(display, pic),
            // PicDataType.Numeric      =>      CobolNumericDecoder.Decode(display, pic),
            _ => throw new NotSupportedException($"Unsupported PIC category: {pic.DataType}")
        };
    }

    /// <summary>
    /// CLR value → DISPLAY
    /// </summary>
    public static string Encode(object value, PicClause pic)
    {
        // 之後實作
        throw new NotImplementedException("Encode is not implemented yet.");
    }
}
