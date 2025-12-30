using System.Text;

namespace GetThePicture.Codec;

public static class EncodingFactory
{
    static EncodingFactory()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    }

    public static Encoding CP950 =>
        Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback,
            DecoderFallback.ExceptionFallback
        );
}
