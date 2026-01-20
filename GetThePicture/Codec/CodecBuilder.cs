using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Decoder;
using GetThePicture.Codec.Encoder;
using GetThePicture.Codec.Options;

namespace GetThePicture.Codec;

public static class CodecBuilder
{
    public static DecodeContext ForPic(PicClause pic) => new(pic);
}

public sealed class DecodeContext(PicClause pic)
{
    private readonly PicClause _pic = pic;
    private readonly CodecOptions _codecOptions = new();

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

    public DecodeContext WithSemantic(PicSemantic picSemantic = PicSemantic.None)
    {
        _pic.Semantic = picSemantic;
        return this;
    }

    public DecodeContext WithUsage(PicUsage Usage = PicUsage.Display)
    {
        _pic.Usage = Usage;
        return this;
    }

    /// <summary>
    /// COBOL PICTURE (buffer) → CLR
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    public object Decode(ReadOnlySpan<byte> buffer)
    {
        if (buffer.Length == 0)
            throw new ArgumentException("Buffer is empty.", nameof(buffer));
        
        return PicDecoder.Decode(buffer, _pic, _codecOptions);
    }

    /// <summary>
    /// CLR → COBOL PICTURE (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public string Encode(object value)
    {
        throw new NotImplementedException("Encode is not implemented yet.");
        
        // ArgumentNullException.ThrowIfNull(value);

        // return PicEecoder.Encode(value, _pic, _codecOptions);
    }
}
