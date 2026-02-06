namespace GetThePicture.Picture.Clause.Encoder.Meta;

/// <summary>
/// Elementary Kind
/// </summary>
internal enum CobMetaType
{
    Text,
    Number
}

/// <summary>
/// COBOL Elementary Meta (For Encoding Purpose)
/// </summary>
internal sealed class CobMeta
{
    public CobMetaType Type { get; }
    public CobMetaText? Text { get; }
    public CobMetaNumber? Number { get; }
    
    private CobMeta(CobMetaType type, CobMetaText? text, CobMetaNumber? number)
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

    public static CobMeta FromText(string text)
    {
        return new(CobMetaType.Text, new CobMetaText(text), null);
    }

    public static CobMeta FromNumber(bool isNegative, string digits, int decimalDigits)
    {
        return new(CobMetaType.Number, null, new CobMetaNumber(isNegative, digits, decimalDigits));
    }

    public static CobMeta FromNumber(string digits)
    {
        return FromNumber(isNegative: false, digits, decimalDigits: 0);
    }
}
