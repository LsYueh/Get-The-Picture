namespace GetThePicture;

public class PicClause
{
    public PicDataType DataType { get; set; }
    public bool Signed { get; set; }

    public int IntegerDigits { get; set; }
    public int DecimalDigits { get; set; }

    public String? Raw { get; set; }

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

            if (DecimalDigits > 0)
                return typeof(decimal);

            // 整數（先簡單）
            return typeof(int);
        }
    }
}
