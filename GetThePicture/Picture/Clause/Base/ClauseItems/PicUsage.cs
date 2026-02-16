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
    /// COMP-3 (Packed-Decimal)
    /// </summary>
    PackedDecimal,
    COMP3 = PackedDecimal,

    /// <summary>
    /// COMP-4 (Binary)
    /// </summary>
    Binary,
    COMP4 = Binary,
    COMP = Binary,

    /// <summary>
    /// COMP-5 (Native Binary)
    /// </summary>
    NativeBinary,
    COMP5 = NativeBinary,

    /// <summary>
    /// COMP-6 (Unsigned Packed-Decimal)
    /// </summary>
    UPackedDecimal,
    COMP6 = UPackedDecimal,
}
