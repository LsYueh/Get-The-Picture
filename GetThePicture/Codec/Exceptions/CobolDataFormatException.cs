namespace GetThePicture.Codec.Exceptions;

public sealed class CobolDataFormatException(
    string message,
    string? pic = null,
    int? offset = null,
    byte? invalidByte = null,
    Exception? innerException = null) : FormatException(message, innerException)
{
    public string? Pic { get; } = pic;
    public int? Offset { get; } = offset;
    public byte? InvalidByte { get; } = invalidByte;

    public override string ToString()
    {
        return $"{Message}" +
                (Pic != null ? $" PIC={Pic}" : "") +
                (Offset != null ? $" Offset={Offset}" : "") +
                (InvalidByte != null ? $" Byte=0x{InvalidByte:X2}" : "");
    }
}

