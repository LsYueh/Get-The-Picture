using GetThePicture.Cobol.Display;

namespace GetThePicture.Cobol.Picture.ComputationalBase;

/// <summary>
/// COMP-3 (packed decimal)
/// </summary>
internal static class COMP3
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
