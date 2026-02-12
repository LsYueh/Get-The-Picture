namespace GetThePicture.Picture.Clause.Base.ClauseItems;

/// <summary>
/// Class extensions for specific semantic meaning of the data.
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

    /// <summary>
    /// PIC X(1) : Y/N or PIC 9(1) : 0/1
    /// </summary>
    Boolean,
}
