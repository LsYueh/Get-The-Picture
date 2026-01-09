namespace GetThePicture.Cobol.Display;

internal sealed class DisplayValue
{
    public DisplayValueKind Kind { get; }
    public DisplayText? Text { get; }
    public DisplayNumber? Number { get; }

    private byte[]? _raw;
    public bool HasRaw => _raw != null;
    
    private DisplayValue(DisplayValueKind kind, DisplayText? text, DisplayNumber? number)
    {
        Kind = kind;
        Text = text;
        Number = number;
    }

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

    public ReadOnlySpan<byte> Raw => _raw ?? throw new InvalidOperationException();

    internal void SetRaw(byte[] raw)
    {
        _raw = raw;
    }
}
