namespace GetThePicture.Cobol;

/// <summary>
/// COBOL PICTURE Data Type
/// </summary>
public enum PicDataType
{
    /// <summary>
    /// 9/S9
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

    /// <summary>
    /// Gregorian calendar (公曆)
    /// </summary>
    Gregorian8,

    /// <summary>
    /// Minguo calendar (民國曆)
    /// </summary
    Minguo7,

    /// <summary>
    /// HHmmss
    /// </summary>
    Time6,

    /// <summary>
    /// HHmmssfff
    /// </summary>
    Time9,
    
    /// <summary>
    /// YYYYMMDDHHmmss
    /// </summary>
    Timestamp14,
}
