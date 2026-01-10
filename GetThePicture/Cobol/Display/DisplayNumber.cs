namespace GetThePicture.Cobol.Display;

internal readonly struct DisplayNumber(bool isNegative, string digits, int decimalDigits)
{
    public bool IsNegative { get; } = isNegative;
    /// <summary>
    /// 純數字，不含符號
    /// </summary>
    public string Digits { get; } = digits;
    public int DecimalDigits { get; } = decimalDigits;
}
