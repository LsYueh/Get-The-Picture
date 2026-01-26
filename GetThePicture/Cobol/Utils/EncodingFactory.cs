using System.Text;

namespace GetThePicture.Cobol.Utils;

internal static class EncodingFactory
{
    public static readonly Encoding StrictCP950;
    public static readonly Encoding CP950;

    static EncodingFactory()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

        StrictCP950 = Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback, // string → byte[]
            DecoderFallback.ExceptionFallback  // byte[] → string
        );

        CP950 = Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback,
            DecoderFallback.ReplacementFallback // 容錯顯示
        );
    }
}
