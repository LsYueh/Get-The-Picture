using GetThePicture.Cobol.Options;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.PictureClause;

/// <summary>
/// Codec Builder
/// </summary>
public static class PicClauseCodec
{
    /// <summary>
    /// 
    /// </summary>
    /// <param name="meta"></param>
    /// <returns></returns>
    public static CodecContext ForMeta(PicMeta meta) => new(meta);
}

/// <summary>
/// COBOL PICTURE Clause Codec Context
/// </summary>
/// <param name="meta">PicMeta</param>
public sealed class CodecContext(PicMeta meta)
{
    private readonly PicMeta _picMeta = meta;
    private readonly CobOptions _cobOptions = new();

    // -------------------------
    // COBOL Compile Options
    // -------------------------

    /// <summary>
    /// Data Length Enforcement
    /// </summary>
    /// <returns></returns>
    public CodecContext WithStrict()
    {
        _cobOptions.Strict = true;
        return this;
    }

    /// <summary>
    /// For PIC S9 DISPLAY. (Overpunch Codex)
    /// </summary>
    /// <param name="opt"></param>
    /// <returns></returns>
    public CodecContext WithDataStorageOption(DataStorageOptions? opt)
    {
        _cobOptions.DataStorage = opt ?? DataStorageOptions.CI;
        return this;
    }

    /// <summary>
    /// For PIC S9 DISPLAY. (Overpunch Codex)
    /// </summary>
    /// <returns></returns>
    public CodecContext WithSignIsLeading()
    {
        _cobOptions.Sign = SignOptions.IsLeading;
        return this;
    }

    /// <summary>
    /// For COMP/COMP-5 use only.
    /// </summary>
    /// <returns></returns>
    public CodecContext WithReversedBinary()
    {
        _cobOptions.Binary = BinaryOptions.Reversed;
        return this;
    }

    // -------------------------
    // COBOL PICTURE Clause 
    // -------------------------

    public CodecContext AsSemantic(PicSemantic picSemantic = PicSemantic.None)
    {
        _picMeta.Semantic = picSemantic;
        return this;
    }

    public CodecContext Usage(PicUsage Usage = PicUsage.Display)
    {
        _picMeta.Usage = Usage;
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
        
        return Decoder.PicDecoder.Decode(buffer, _picMeta, _cobOptions);
    }

    /// <summary>
    /// CLR → COBOL Elementary Item (buffer)
    /// </summary>
    /// <param name="value"></param>
    /// <returns></returns>
    public byte[] Encode(object value)
    {
        ArgumentNullException.ThrowIfNull(value);

        return Encoder.PicEncoder.Encode(value, _picMeta, _cobOptions);
    }
}
