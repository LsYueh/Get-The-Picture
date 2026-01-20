using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Codec.Options;

public sealed class CodecOptions
{
    /// <summary>
    /// 嚴格長度驗證 (預設 false)
    /// </summary>
    public bool Strict { get; set; } = false;

    /// <summary>
    /// `-Dci` is the default.
    /// </summary>
    public DataStorageOptions DataStorage { get; set; } = DataStorageOptions.CI;

    /// <summary>
    /// `SIGN IS TRAILING` is the default.
    /// </summary>
    public SignOptions Sign { get; set; } = SignOptions.IsTrailing;

    /// <summary>
    /// BINARY specifies the representation format of binary data items.
    /// <br/>
    /// Ref. <see href="https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=options-binary">BINARY</see>
    /// </summary>
    public BinaryOptions Binary { get; set; } = BinaryOptions.Normal;
}
