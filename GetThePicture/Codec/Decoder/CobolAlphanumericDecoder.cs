using GetThePicture.Pic;

namespace GetThePicture.Codec.Decoder;

internal static class CobolAlphanumericDecoder
{
    public static string Decode(string display, PicClause pic)
    {
        // X(n) DISPLAY 通常右補空白
        return display.TrimEnd();
    }
}
