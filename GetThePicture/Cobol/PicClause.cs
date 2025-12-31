namespace GetThePicture.Cobol;

/// <summary>
/// COBOL PICTURE Metadata Class
/// </summary>
public class PicClause
{
    public PicDataType DataType { get; set; }
    
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
        return $"{DataType}, Signed={Signed}, Int={IntegerDigits}, Dec={DecimalDigits}, Len={TotalLength}";
    }

    public Type ClrType
    {
        get
        {
            if (DataType == PicDataType.Alphanumeric)
                return typeof(string);

            if (DataType == PicDataType.Alphabetic)
                return typeof(string);

            if (DecimalDigits > 0)
                return typeof(decimal);

            // 整數（先簡單）
            return typeof(int);
        }
    }
}
