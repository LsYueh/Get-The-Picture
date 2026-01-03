using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Decoder;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec;

public static class CobolValueCodec
{
    public static DecodeContext Build(string display, PicClause pic) => new(display, pic);
}

public sealed class DecodeContext
{
    private readonly string _display;
    private readonly PicClause _pic;
    private readonly CodecOptions _codecOptions = new();


    internal DecodeContext(string display, PicClause pic)
    {
        _display = display;
        _pic = pic;
    }

    public DecodeContext NoStrict()
    {
        _codecOptions.Strict = false;
        return this;
    }

    public DecodeContext WithDataStorageOption(DataStorageOptions? opt)
    {
        _codecOptions.DataStorage = opt ?? DataStorageOptions.CI;
        return this;
    }

    public DecodeContext WithSignIsLeading()
    {
        _codecOptions.Sign = SignOptions.IsLeading;
        return this;
    }

    public object Decode() => Codec.Decode(_display, _pic, _codecOptions);
}

internal static class Codec
{
    /// <summary>
    /// COBOL PICTURE DISPLAY → CLR value
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

    /// <summary>
    /// CLR value → COBOL PICTURE DISPLAY
    /// </summary>
    public static string Encode(object value, PicClause pic)
    {
        // 之後實作
        throw new NotImplementedException("Encode is not implemented yet.");
    }
}
