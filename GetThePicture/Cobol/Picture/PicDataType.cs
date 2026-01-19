namespace GetThePicture.Cobol.Picture;

/// <summary>
/// Represents COBOL PIC base types.
/// </summary>
public enum PicBaseClass
{
    /// <summary>
    /// 9 / S9
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
/// Domain-specific semantic storage formats
/// (e.g. dates, times, timestamps).
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
/// 
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

/// <summary>
/// Little-endian / Big-endian Switchs
/// </summary>
public enum BinaryOptions
{
    /// <summary>
    /// 
    /// </summary>
    Normal,

    /// <summary>
    /// 
    /// </summary>
    Reversed,
}
