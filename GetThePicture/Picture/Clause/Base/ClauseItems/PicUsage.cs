namespace GetThePicture.Picture.Clause.Base.ClauseItems;

/// <summary>
/// USAGE clause
/// </summary>
public enum PicUsage
{
    /// <summary>
    /// DISPLAY (default)
    /// </summary>
    Display,

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

    /// <summary>
    /// COMP-6 (Unsigned Packed Decimal)
    /// </summary>
    UPackedDecimal,
}
