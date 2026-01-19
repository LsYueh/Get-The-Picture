using GetThePicture.Cobol.Picture.ComputationalBase;

namespace GetThePicture.Cobol.Picture;

/// <summary>
/// Physical storage format for PICTURE 9/S/V/P.
/// </summary>
public enum ComputationalItems
{
    /// <summary>
    /// As DISPLAY (default)
    /// </summary>
    None,

    /// <summary>
    /// COMP (binary)
    /// </summary>
    Binary,

    /// <summary>
    /// COMP-3 (packed decimal)
    /// </summary>
    PackedDecimal,

    /// <summary>
    /// COMP-5 (native binary)
    /// </summary>
    NativeBinary,
}

/// <summary>
/// <br/>
/// <see href="https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=clause-computational-items">Computational items</see>
/// </summary>
public static class Computational
{
    public static string Decode(ReadOnlySpan<byte> buffer, PicClause pic)
    {
        if (pic.BaseType != PicBaseType.Numeric)
            throw new InvalidOperationException("COMP only supports numeric PIC.");

        return pic.Comp switch
        {
            ComputationalItems.None          => Display_Decode(buffer, pic),
            ComputationalItems.Binary        =>    COMP.Decode(buffer, pic),
            ComputationalItems.PackedDecimal =>   COMP3.Decode(buffer, pic),
            ComputationalItems.NativeBinary  =>   COMP5.Decode(buffer, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Comp}")
        };
    }

    public static byte[] Encode(string text, PicClause pic)
    {
        if (pic.BaseType != PicBaseType.Numeric)
            throw new InvalidOperationException("COMP only supports numeric PIC.");

        return pic.Comp switch
        {
            ComputationalItems.None          => Display_Encode(text, pic),
            ComputationalItems.Binary        =>    COMP.Encode(text, pic),
            ComputationalItems.PackedDecimal =>   COMP3.Encode(text, pic),
            ComputationalItems.NativeBinary  =>   COMP5.Encode(text, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Comp}")
        };
    }

    private static string Display_Decode(ReadOnlySpan<byte> buffer, PicClause pic)
    {
        throw new NotImplementedException();
    }

    private static byte[] Display_Encode(string text, PicClause pic)
    {
        throw new NotImplementedException();
    }
}