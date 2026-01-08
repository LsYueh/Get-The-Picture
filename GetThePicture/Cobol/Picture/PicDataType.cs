namespace GetThePicture.Cobol.Picture;

/// <summary>
/// Represents COBOL PIC base types.
/// </summary>
public enum PicBaseType
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
