namespace GetThePicture.Cobol.Picture.TypeBase;

/// <summary>
/// Class of elementary data items.
/// </summary>
public enum PicBaseClass
{
    /// <summary>
    /// 9/S/V
    /// </summary>
    Numeric,

    /// <summary>
    /// X
    /// </summary>
    Alphanumeric,

    /// <summary>
    /// A
    /// </summary>
    Alphabetic,
}

/// <summary>
/// As Category
/// </summary>
public enum PicSemantic
{
    /// <summary>
    /// No additional semantic meaning.
    /// </summary>
    None,

    /// <summary>
    /// Gregorian calendar date (YYYYMMDD)
    /// </summary>
    GregorianDate,

    /// <summary>
    /// Minguo calendar date (YYYMMDD)
    /// </summary>
    MinguoDate,

    /// <summary>
    /// Time (HHmmss)
    /// </summary>
    Time6,

    /// <summary>
    /// Time with milliseconds (HHmmssfff)
    /// </summary>
    Time9,

    /// <summary>
    /// Timestamp (YYYYMMDDHHmmss)
    /// </summary>
    Timestamp14,
}

/// <summary>
/// Usage
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
}
