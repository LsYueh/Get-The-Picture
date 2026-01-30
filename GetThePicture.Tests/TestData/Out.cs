namespace GeneratedCopybook;

/// <summary>
/// Record Size : 3 <br />
/// </summary>
public sealed class EmployeeRecord
{
    /// <summary>
    /// X [1]
    /// </summary>
    public string EmpStatus { get; set; } = null!;
    public bool IsActive => EmpStatus == "A";
    public bool IsInactive => EmpStatus == "I";
    public bool IsOnLeave => EmpStatus == "L";

    /// <summary>
    /// X [1]
    /// </summary>
    public string EmpType { get; set; } = null!;

    /// <summary>
    /// 9 [1]
    /// </summary>
    public byte EmpLevel { get; set; }

}
