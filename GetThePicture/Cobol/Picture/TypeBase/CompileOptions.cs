namespace GetThePicture.Cobol.Picture.TypeBase;

public enum DataStorageOptions
{
    /// <summary>RM/COBOL (not RM/COBOL-85)</summary>
    CA,
    /// <summary>MBP COBOL</summary>
    CB,
    /// <summary>IBM COBOL (RM/COBOL-85)</summary>
    CI,
    /// <summary>Micro Focus COBOL</summary>
    CM,
    /// <summary>NCR COBOL</summary>
    CN,
    /// <summary>Realia COBOL</summary>
    CR,
    /// <summary>VAX COBOL</summary>
    CV,
}

/// <summary>
/// `SIGN IS TRAILING` is the default.
/// </summary>
public enum SignOptions {
    IsTrailing, // default
    IsLeading,
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
