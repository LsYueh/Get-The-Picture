using GetThePicture.Picture.Base.Clause.Items;

namespace GetThePicture.Picture.Base.Symbols.Core;

public sealed class SymbolMeta(
    PicBaseClass baseClass = PicBaseClass.Unknown, bool signed = false,
    int integerDigits = 0, int decimalDigits = 0
) 
{
    public PicBaseClass BaseClass { get; set; } = baseClass;

    public bool Signed { get; set; } = signed;

    /// <summary>
    /// 字串長度/整數位數
    /// </summary>
    public int IntegerDigits { get; set; } = integerDigits;

    /// <summary>
    /// 小數位數
    /// </summary>
    public int DecimalDigits { get; set; } = decimalDigits;
}