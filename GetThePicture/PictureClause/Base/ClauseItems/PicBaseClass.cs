namespace GetThePicture.PictureClause.Base.ClauseItems;

/// <summary>
/// Class of elementary items.
/// </summary>
public enum PicBaseClass
{
    /// <summary>
    /// Consists of only digits (0-9), and possibly an operational sign and a decimal point. Defined with PIC 9 or S9. <br/>
    /// Numeric items can also have different internal USAGE such as PACKED-DECIMAL (COMP-3), BINARY (COMP), COMP-1, COMP-2, etc..
    /// </summary>
    Numeric,

    /// <summary>
    /// Consists of any character supported by the system's character set (digits, letters, special characters). Defined with PIC X.
    /// </summary>
    Alphanumeric,

    /// <summary>
    /// Consists of only letters (A-Z, a-z) and spaces. Defined with PIC A.
    /// </summary>
    Alphabetic,
}
