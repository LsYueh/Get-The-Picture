using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Symbols.Base;

public sealed class SymbolMeta(
    PicBaseClass baseClass = PicBaseClass.Unknown, bool signed = false,
    int intDigits = 0, int decDigits = 0
) 
{
    public PicBaseClass BaseClass { get; init; } = baseClass;

    public bool Signed { get; init; } = signed;

    /// <summary>
    /// 字串長度/整數位數
    /// </summary>
    public int IntegerDigits { get; set; } = intDigits;

    /// <summary>
    /// 小數位數
    /// </summary>
    public int DecimalDigits { get; set; } = decDigits;
}