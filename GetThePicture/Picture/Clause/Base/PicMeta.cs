using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Base;

/// <summary>
/// COBOL PICTURE Metadata Class
/// </summary>
public class PicMeta
{
    public string Raw { get; init; } = null!;

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
    /// COBOL-PIC 記憶體佔用資料的長度
    /// </summary>
    public int StorageOccupied =>
        Usage switch
        {
            PicUsage.Display => DigitCount,
            PicUsage.COMP3 or
            PicUsage.COMP6   => Computational.COMP3.GetByteLength(DigitCount),
            PicUsage.COMP4   => Computational.COMP5.GetByteLength(DigitCount),
            PicUsage.COMP5   => Computational.COMP5.GetByteLength(DigitCount),
            _ => throw new NotSupportedException()
        };

    public override string ToString()
    {
        return $"[{Raw}] Class='{BaseClass}' (Semantic='{Semantic}'), Signed={Signed}, Int={IntegerDigits}, Dec={DecimalDigits}, Len={DigitCount}, Usage='{Usage}'";
    }

    public static PicMeta Parse(
        string input, PicSemantic semantic = PicSemantic.None, PicUsage Usage = PicUsage.Display
    ) => PicMetaBuilder.Parse(input, semantic, Usage);
}
