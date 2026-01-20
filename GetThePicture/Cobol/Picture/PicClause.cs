using GetThePicture.Cobol.Picture.ComputationalBase;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Cobol.Picture;

/// <summary>
/// COBOL PICTURE Metadata Class
/// </summary>
public class PicClause
{
    public PicBaseClass BaseClass { get; set; }
    public PicSemantic Semantic { get; set; }

    public PicUsage Usage { get; set; } = PicUsage.Display;
    
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
    /// COBOL-PIC 宣告資料的長度
    /// </summary>
    public int DigitCount => IntegerDigits + DecimalDigits;

    /// <summary>
    /// COBOL-PIC 總佔用資料的長度
    /// </summary>
    public int StorageOccupied =>
        Usage switch
        {
            PicUsage.Display       => DigitCount,
            PicUsage.Binary        => COMP5.GetByteLength(this),
            PicUsage.PackedDecimal => (DigitCount + 1) / 2,
            PicUsage.NativeBinary  => COMP5.GetByteLength(this),
            _ => throw new NotSupportedException()
        };

    public override string ToString()
    {
        return $"{BaseClass} ({Semantic}), Signed={Signed}, Int={IntegerDigits}, Dec={DecimalDigits}, Len={DigitCount}";
    }
}
