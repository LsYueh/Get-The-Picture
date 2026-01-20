using GetThePicture.Cobol.Elementary;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Cobol.Picture.ComputationalBase;

/// <summary>
/// COMP-5 (native binary)
/// </summary>
internal static class COMP5
{
    public static object Decode(ReadOnlySpan<byte> buffer, PicClause pic, BinaryOptions endian = BinaryOptions.Normal)
    {
        int length = GetByteLength(pic);
        
        if (buffer.Length < length)
            throw new ArgumentException("Buffer too short");

        Span<byte> bytes = stackalloc byte[length];
        buffer[..length].CopyTo(bytes);

        if (endian == BinaryOptions.Reversed)
            bytes.Reverse();

        return length switch
        {
            2 => pic.Signed ? BitConverter.ToInt16(bytes) : BitConverter.ToUInt16(bytes),
            4 => pic.Signed ? BitConverter.ToInt32(bytes) : BitConverter.ToUInt32(bytes),
            8 => pic.Signed ? BitConverter.ToInt64(bytes) : BitConverter.ToUInt64(bytes),
            _ => throw new NotSupportedException("Unsupported COMP length")
        };
    }

    public static byte[] Encode(ElementaryMeta displayValue, PicClause pic, BinaryOptions endian = BinaryOptions.Normal)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException($"COMP does not support decimal digits. PIC has {pic.DecimalDigits} decimal digits.");
        
        if (displayValue.Number is null)
            throw new ArgumentNullException(nameof(displayValue));

        var number = displayValue.Number.Value;

        // 檢查是否有小數位
        if (number.Value != decimal.Truncate(number.Value))
            throw new InvalidOperationException($"COMP Encode can only handle integers. Value {number.Value} has fractional part.");

        int length = GetByteLength(pic);

        // 範圍檢查
        switch (length)
        {
            case 2:
                if (pic.Signed && (number.Value < short.MinValue || number.Value > short.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 2-byte signed range.");
                if (!pic.Signed && (number.Value < 0 || number.Value > ushort.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 2-byte unsigned range.");
                break;

            case 4:
                if (pic.Signed && (number.Value < int.MinValue || number.Value > int.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 4-byte signed range.");
                if (!pic.Signed && (number.Value < 0 || number.Value > uint.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 4-byte unsigned range.");
                break;

            case 8:
                if (pic.Signed && (number.Value < long.MinValue || number.Value > long.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 8-byte signed range.");
                if (!pic.Signed && (number.Value < 0 || number.Value > ulong.MaxValue))
                    throw new OverflowException($"Value {number.Value} exceeds 8-byte unsigned range.");
                break;

            default:
                throw new NotSupportedException("Unsupported COMP length");
        }

        Span<byte> bytes = length switch
        {
            2 => pic.Signed ? BitConverter.GetBytes((short)number.Value) : BitConverter.GetBytes((ushort)number.Value),
            4 => pic.Signed ? BitConverter.GetBytes((int)  number.Value) : BitConverter.GetBytes((uint)  number.Value),
            8 => pic.Signed ? BitConverter.GetBytes((long) number.Value) : BitConverter.GetBytes((ulong) number.Value),
            _ => throw new NotSupportedException()
        };

        if (endian == BinaryOptions.Reversed)
            bytes.Reverse();

        return bytes.ToArray();
    }

    public static int GetByteLength(PicClause pic)
    {
        int digits = pic.DigitCount;

        return digits switch
        {
            <=  4 => 2,
            <=  9 => 4,
            <= 18 => 8,
            _ => throw new NotSupportedException("Too many digits for COMP/BINARY")
        };
    }
}
