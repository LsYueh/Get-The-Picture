namespace GetThePicture.Cobol.Picture;

/// <summary>
/// Physical storage format for PICTURE 9/S/V/P.
/// </summary>
public enum ComputationalItem
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
public static class COMP
{
    // TODO: DeCOMP / EnCOMP ...
}