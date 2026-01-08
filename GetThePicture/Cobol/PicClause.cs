namespace GetThePicture.Cobol;

/// <summary>
/// COBOL PICTURE Metadata Class
/// </summary>
public class PicClause
{
    public PicBaseType BaseType { get; set; }
    public PicSemantic Semantic { get; set; }
    
    public bool Signed { get; set; } = false;

    /// <summary>
    /// 字串長度/整數位數
    /// </summary>
    public int IntegerDigits { get; set; } = 0;

    /// <summary>
    /// 小數位數
    /// </summary>
    public int DecimalDigits { get; set; } = 0;

    /// <summary>
    /// COBOL-PIC 總佔用資料的長度
    /// </summary>
    public int TotalLength => IntegerDigits + DecimalDigits;

    public override string ToString()
    {
        return $"{BaseType} ({Semantic}), Signed={Signed}, Int={IntegerDigits}, Dec={DecimalDigits}, Len={TotalLength}";
    }
}
