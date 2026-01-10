namespace GetThePicture.Cobol.Display;

internal sealed class DisplayValue
{
    public DisplayValueKind Kind { get; }
    public DisplayText? Text { get; }
    public DisplayNumber? Number { get; }
    
    private DisplayValue(DisplayValueKind kind, DisplayText? text, DisplayNumber? number)
    {
        Kind = kind;
        Text = text;
        Number = number;
    }

    public bool IsNegative => Number?.IsNegative == true;

    public decimal Sign => IsNegative ? -1.0m : 1.0m;

    /// <summary>
    /// 可用於 Encode / 格式化的數值字串表示
    /// </summary>
    public string NumericText => Number?.Digits ?? Text?.Value ?? string.Empty;

    public static DisplayValue FromText(string text)
    {
        return new(DisplayValueKind.Text, new DisplayText(text), null);
    }

    public static DisplayValue FromNumber(bool isNegative, string digits, int decimalDigits)
    {
        return new(DisplayValueKind.Number, null, new DisplayNumber(isNegative, digits, decimalDigits));
    }

    public static DisplayValue FromNumber(string digits)
    {
        return FromNumber(isNegative: false, digits, decimalDigits: 0);
    }
}
