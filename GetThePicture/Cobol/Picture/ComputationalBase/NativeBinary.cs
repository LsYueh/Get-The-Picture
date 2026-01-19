using GetThePicture.Cobol.Display;

namespace GetThePicture.Cobol.Picture.ComputationalBase;

/// <summary>
/// COMP-5 (native binary)
/// </summary>
internal static class COMP5
{
    public static string Decode(ReadOnlySpan<byte> buffer, PicClause pic)
    {
        throw new NotImplementedException();
    }

    public static byte[] Encode(DisplayValue displayValue, PicClause pic)
    {
        throw new NotImplementedException();
    }
}
