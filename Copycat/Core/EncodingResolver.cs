using System.Text;

namespace Copycat.Core;

public static class EncodingResolver
{
    static EncodingResolver()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
    }

    private static readonly Lazy<Encoding> _cp950 =
        new(() => Encoding.GetEncoding(
            950,
            EncoderFallback.ExceptionFallback,
            DecoderFallback.ReplacementFallback
        ));

    public static Encoding CP950 => _cp950.Value;
}
