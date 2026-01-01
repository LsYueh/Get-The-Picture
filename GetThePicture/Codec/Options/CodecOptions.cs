using GetThePicture.Cobol;

namespace GetThePicture.Codec.Options;

public sealed class CodecOptions
{
    /// <summary>
    /// `-Dci` is the default.
    /// </summary>
    public DataStorageOptions DataStorage { get; set; } = DataStorageOptions.CI;

    /// <summary>
    /// `SIGN IS TRAILING` is the default.
    /// </summary>
    public SignOptions Sign { get; set; } = SignOptions.IsTrailing;
}
