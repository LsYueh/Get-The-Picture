namespace GetThePicture.Cobol.Display;

internal readonly struct DisplayNumber(bool isNegative, string digits, int decimalDigits, decimal? value = null)
{
    public bool IsNegative { get; } = isNegative;
    /// <summary>
    /// 純數字，不含符號
    /// </summary>
    public string Digits { get; } = digits;
    public int DecimalDigits { get; } = decimalDigits;

    /// <summary>
    /// 原始數值，方便計算
    /// </summary>
    public decimal Value { get; } = value ?? ParseDecimal(isNegative, digits, decimalDigits);

    private static decimal ParseDecimal(bool isNegative, string digits, int decimalDigits)
    {
        if (string.IsNullOrEmpty(digits)) return 0m;

        decimal result = decimal.Parse(digits);
        if (decimalDigits > 0)
            result /= (decimal)Math.Pow(10, decimalDigits);

        if (isNegative) result = -result;

        return result;
    }
}
