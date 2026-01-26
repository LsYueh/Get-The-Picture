using GetThePicture.Cobol.Options;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause;

public static class CodecBuilder
{
    public static DecodeContext ForPic(PicClause pic) => new(pic);
}

public sealed class DecodeContext(PicClause pic)
{
    private readonly PicClause _pic = pic;
    private readonly CodecOptions _codecOptions = new();

    // -------------------------
    // COBOL Compile Options
    // -------------------------

    /// <summary>
    /// Data Length Enforcement
    /// </summary>
    /// <returns></returns>
    public DecodeContext WithStrict()
    {
        _codecOptions.Strict = true;
        return this;
    }

    /// <summary>
    /// For PIC S9 DISPLAY. (Overpunch Codex)
    /// </summary>
    /// <param name="opt"></param>
    /// <returns></returns>
    public DecodeContext WithDataStorageOption(DataStorageOptions? opt)
    {
        _codecOptions.DataStorage = opt ?? DataStorageOptions.CI;
        return this;
    }

    /// <summary>
    /// For PIC S9 DISPLAY. (Overpunch Codex)
    /// </summary>
    /// <returns></returns>
    public DecodeContext WithSignIsLeading()
    {
        _codecOptions.Sign = SignOptions.IsLeading;
        return this;
    }

    /// <summary>
    /// For COMP/COMP-5 use only.
    /// </summary>
    /// <returns></returns>
    public DecodeContext WithReversedBinary()
    {
        _codecOptions.Binary = BinaryOptions.Reversed;
        return this;
    }

    // -------------------------
    // COBOL PICTURE Clause 
    // -------------------------

    public DecodeContext AsSemantic(PicSemantic picSemantic = PicSemantic.None)
    {
        _pic.Semantic = picSemantic;
        return this;
    }

    public DecodeContext Usage(PicUsage Usage = PicUsage.Display)
    {
        _pic.Usage = Usage;
        return this;
    }

    // -------------------------
    // Codec
    // -------------------------

    /// <summary>
    /// COBOL Elementary Item (buffer) → CLR
    /// </summary>
    /// <param name="buffer">COBOL Elementary Item</param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    public object Decode(ReadOnlySpan<byte> buffer)
    {
        if (buffer.Length == 0)
            throw new ArgumentException("Buffer is empty.", nameof(buffer));
        
        return Decoder.PicDecoder.Decode(buffer, _pic, _codecOptions);
    }

    /// <summary>
    /// CLR → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public byte[] Encode(object value)
    {
        ArgumentNullException.ThrowIfNull(value);

        return Encoder.PicEncoder.Encode(value, _pic, _codecOptions);
    }
}
