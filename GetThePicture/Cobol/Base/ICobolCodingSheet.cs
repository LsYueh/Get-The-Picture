namespace GetThePicture.Cobol.Base;

/// <summary>
/// COBOL coding sheet
/// </summary>
/// <param name="lineNumber"></param>
public interface ICobolCodingSheet
{    
    /// <summary>
    /// Column 1-6
    /// </summary>
    public string Sequence { get; }

    /// <summary>
    /// Column 7
    /// </summary>
    public char Indicator { get; }

    /// <summary>
    /// Column 8-11
    /// </summary>
    public string AreaA { get; }

    /// <summary>
    /// Column 12-72
    /// </summary>
    public string AreaB { get; }

    /// <summary>
    /// Column 73-80（可選）
    /// </summary>
    public string Remark { get; }
}
