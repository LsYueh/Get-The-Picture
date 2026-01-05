using GetThePicture.Cobol;
using GetThePicture.Codec.Decoder;
using GetThePicture.Codec.Encoder;
using GetThePicture.Codec.Options;

namespace GetThePicture.Codec;

public static class CobolValueCodec
{
    public static DecodeContext ForPic(PicClause pic) => new(pic);
}

public sealed class DecodeContext
{
    private readonly PicClause _pic;
    private readonly CodecOptions _codecOptions = new();

    internal DecodeContext(PicClause pic)
    {
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

    /// <summary>
    /// 
    /// </summary>
    /// <param name="display"></param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    public object Decode(string display)
    {
        ArgumentNullException.ThrowIfNull(display);
        
        return CobolPicDecoder.Decode(display, _pic, _codecOptions);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public string Encode(object value)
    {
        ArgumentNullException.ThrowIfNull(value);

        return CobolPicEecoder.Encode(value, _pic, _codecOptions);
    }
}
