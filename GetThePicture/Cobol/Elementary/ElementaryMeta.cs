namespace GetThePicture.Cobol.Elementary;

/// <summary>
/// Elementary Kind
/// </summary>
internal enum EleType
{
    Text,
    Number
}

/// <summary>
/// Elementary Meta
/// </summary>
internal sealed class ElementaryMeta
{
    public EleType Type { get; }
    public EleText? Text { get; }
    public EleNumber? Number { get; }
    
    private ElementaryMeta(EleType type, EleText? text, EleNumber? number)
    {
        Type = type;
        Text = text;
        Number = number;
    }

    public bool IsNegative => Number?.IsNegative == true;

    public decimal Sign => IsNegative ? -1.0m : 1.0m;

    /// <summary>
    /// 可用於 Encode / 格式化的數值字串表示
    /// </summary>
    public string NumericText => Number?.Digits ?? Text?.Value ?? string.Empty;

    public static ElementaryMeta FromText(string text)
    {
        return new(EleType.Text, new EleText(text), null);
    }

    public static ElementaryMeta FromNumber(bool isNegative, string digits, int decimalDigits)
    {
        return new(EleType.Number, null, new EleNumber(isNegative, digits, decimalDigits));
    }

    public static ElementaryMeta FromNumber(string digits)
    {
        return FromNumber(isNegative: false, digits, decimalDigits: 0);
    }
}
