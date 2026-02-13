using GetThePicture.Picture.Clause.Base.Options;
using static GetThePicture.Picture.Clause.Encoder.Category.NumericEncoder;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP-5 (native binary)
/// </summary>
internal static class COMP5
{
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, BinaryOptions endian = BinaryOptions.Normal)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException($"COMP-5 does not support decimal digits. PIC has {pic.DecimalDigits} decimal digits.");
        
        int length = GetByteLength(pic);
        
        if (buffer.Length < length)
            throw new ArgumentException("Buffer too short");

        Span<byte> bytes = stackalloc byte[length];
        buffer[..length].CopyTo(bytes);

        if (endian == BinaryOptions.Reversed)
            bytes.Reverse();

        return length switch
        {
            // Binary halfword (2 bytes)
            2 => pic.Signed ? BitConverter.ToInt16(bytes) : BitConverter.ToUInt16(bytes),
            // Binary fullword (4 bytes)
            4 => pic.Signed ? BitConverter.ToInt32(bytes) : BitConverter.ToUInt32(bytes),
            // Binary doubleword (8 bytes)
            8 => pic.Signed ? BitConverter.ToInt64(bytes) : BitConverter.ToUInt64(bytes),

            _ => throw new NotSupportedException("Unsupported COMP length")
        };
    }

    public static byte[] Encode(NumericMeta nMeta, PicMeta pic, BinaryOptions endian = BinaryOptions.Normal)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException($"COMP-5 does not support decimal digits. PIC has {pic.DecimalDigits} decimal digits.");

        decimal value = nMeta.Value;

        // 檢查是否有小數位
        if (value != decimal.Truncate(value))
            throw new InvalidOperationException($"COMP-5 Encode can only handle integers. Value {value} has fractional part.");

        int length = GetByteLength(pic);

        // 範圍檢查
        switch (length)
        {
            case 2: // Binary halfword (2 bytes)
                if (pic.Signed && (value < short.MinValue || value > short.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 2-byte signed range.");
                if (!pic.Signed && (value < 0 || value > ushort.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 2-byte unsigned range.");
                break;

            case 4: // Binary fullword (4 bytes)
                if (pic.Signed && (value < int.MinValue || value > int.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 4-byte signed range.");
                if (!pic.Signed && (value < 0 || value > uint.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 4-byte unsigned range.");
                break;

            case 8: // Binary doubleword (8 bytes)
                if (pic.Signed && (value < long.MinValue || value > long.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 8-byte signed range.");
                if (!pic.Signed && (value < 0 || value > ulong.MaxValue))
                    throw new OverflowException($"Value {value} exceeds 8-byte unsigned range.");
                break;

            default:
                throw new NotSupportedException("Unsupported COMP-5 length");
        }

        Span<byte> bytes = length switch
        {
            // Binary halfword (2 bytes)
            2 => pic.Signed ? BitConverter.GetBytes((short)value) : BitConverter.GetBytes((ushort)value),
            // Binary fullword (4 bytes)
            4 => pic.Signed ? BitConverter.GetBytes((int)  value) : BitConverter.GetBytes((uint)  value),
            // Binary doubleword (8 bytes)
            8 => pic.Signed ? BitConverter.GetBytes((long) value) : BitConverter.GetBytes((ulong) value),

            _ => throw new NotSupportedException()
        };

        if (endian == BinaryOptions.Reversed)
            bytes.Reverse();

        return bytes.ToArray();
    }

    public static int GetByteLength(PicMeta pic)
    {
        int digits = pic.DigitCount;

        return digits switch
        {
            <=  4 => 2,
            <=  9 => 4,
            <= 18 => 8,
            _ => throw new NotSupportedException("Too many digits for COMP-5 (Native-Binary)")
        };
    }
}
