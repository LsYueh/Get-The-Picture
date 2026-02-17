using System.Buffers.Binary;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP-5 (Native Binary) <br/>
/// - Uses platform-native endian. <br/>
/// - Little Endian on x86/x64, Big Endian on mainframe. <br/>
/// - Cross-platform use: handle endian conversion as needed. <br/>
/// - For consistency with COMP-4/COMP-3, codec may optionally normalize to Big Endian. <br/>
/// </summary>
internal static class COMP5
{
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, bool isBigEndian = true)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException($"COMP-5 does not support decimal digits. PIC has {pic.DecimalDigits} decimal digits.");
        
        int length = GetByteLength(pic.DigitCount);
        
        if (buffer.Length < length)
            throw new ArgumentException("Buffer too short");

        Span<byte> bytes = stackalloc byte[length];
        buffer[..length].CopyTo(bytes);

        if (BitConverter.IsLittleEndian && isBigEndian)
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

    public static byte[] Encode(NumericMeta nMeta, PicMeta pic, bool isBigEndian = true)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException("COMP-5 does not support decimal digits.");
        
        int length = GetByteLength(pic.DigitCount);
        byte[] bytes = new byte[length];
        
        if (pic.Signed)
        {
            long value = nMeta.ToInt64();
            WriteSigned(bytes, value, length, isBigEndian);
        }
        else
        {
            ulong value = nMeta.ToUInt64();
            WriteUnsigned(bytes, value, length, isBigEndian);
        }

        return bytes;
    }

    public static int GetByteLength(int digitCount)
    {
        return digitCount switch
        {
            <=  4 => 2,
            <=  9 => 4,
            <= 18 => 8,
            _ => throw new NotSupportedException("Too many digits for COMP-4 (Binary) or COMP-5 (Native-Binary)")
        };
    }

    private static void WriteSigned(Span<byte> buffer, long value, int length, bool isBigEndian)
    {
        switch (length)
        {
            case 2:
                if (value < short.MinValue || value > short.MaxValue)
                    throw new OverflowException("Value exceeds 2-byte signed range.");

                if (isBigEndian)
                    BinaryPrimitives.WriteInt16BigEndian(buffer, (short)value);
                else
                    BinaryPrimitives.WriteInt16LittleEndian(buffer, (short)value);
                break;

            case 4:
                if (value < int.MinValue || value > int.MaxValue)
                    throw new OverflowException("Value exceeds 4-byte signed range.");

                if (isBigEndian)
                    BinaryPrimitives.WriteInt32BigEndian(buffer, (int)value);
                else
                    BinaryPrimitives.WriteInt32LittleEndian(buffer, (int)value);
                break;

            case 8:
                if (isBigEndian)
                    BinaryPrimitives.WriteInt64BigEndian(buffer, value);
                else
                    BinaryPrimitives.WriteInt64LittleEndian(buffer, value);
                break;

            default:
                throw new NotSupportedException("Unsupported COMP-5 length");
        }
    }

    private static void WriteUnsigned(Span<byte> buffer, ulong value, int length, bool isBigEndian)
    {
        switch (length)
        {
            case 2:
                if (value > ushort.MaxValue)
                    throw new OverflowException("Value exceeds 2-byte unsigned range.");

                if (isBigEndian)
                    BinaryPrimitives.WriteUInt16BigEndian(buffer, (ushort)value);
                else
                    BinaryPrimitives.WriteUInt16LittleEndian(buffer, (ushort)value);
                break;

            case 4:
                if (value > uint.MaxValue)
                    throw new OverflowException("Value exceeds 4-byte unsigned range.");

                if (isBigEndian)
                    BinaryPrimitives.WriteUInt32BigEndian(buffer, (uint)value);
                else
                    BinaryPrimitives.WriteUInt32LittleEndian(buffer, (uint)value);
                break;

            case 8:
                if (isBigEndian)
                    BinaryPrimitives.WriteUInt64BigEndian(buffer, value);
                else
                    BinaryPrimitives.WriteUInt64LittleEndian(buffer, value);
                break;

            default:
                throw new NotSupportedException("Unsupported COMP-5 length");
        }
    }
}
