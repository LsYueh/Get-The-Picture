using System.Text;

namespace GetThePicture.PictureClause.Utils;

public static class EncodingFactory
{
    private static readonly Lazy<Encoding> _strictCP950 =
        new(() => Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback, // string → byte[]
            DecoderFallback.ExceptionFallback  // byte[] → string
        ));

    private static readonly Lazy<Encoding> _cp950 =
        new(() => Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback,
            DecoderFallback.ReplacementFallback // 容錯顯示
        ));

    static EncodingFactory()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    }

    public static Encoding StrictCP950 => _strictCP950.Value;
    public static Encoding CP950 => _cp950.Value;
}
