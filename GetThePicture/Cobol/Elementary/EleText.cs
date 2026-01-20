namespace GetThePicture.Cobol.Elementary;

internal readonly struct EleText(string value)
{
    public string Value { get; } = value;
}
