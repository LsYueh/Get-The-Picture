namespace GetThePicture.Cobol.Display;

internal readonly struct DisplayText(string value)
{
    public string Value { get; } = value;
}
