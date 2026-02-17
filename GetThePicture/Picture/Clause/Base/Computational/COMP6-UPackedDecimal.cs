using GetThePicture.Picture.Clause.Base.Computational.Base;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;
using GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP-6 (Unsigned Packed-Decimal)
/// </summary>
internal static class COMP6
{
    // Unsigned Packed Decimal (COMP-6) Bit / Nibble Format
    //  
    // Byte n-2           Byte n-1 (last)
    // +--------+--------+--------+--------+
    // |  Digit |  Digit |  Digit |  Sign  |
    // |  4bit  |  4bit  |  4bit  |  4bit  |
    // +--------+--------+--------+--------+
    //    High     Low      High      Low
    // +--------+--------+--------+--------+
    // |  (MSN) |        |  (LSN) |        |
    // +--------+--------+--------+--------+
    // |      (MSB)      |      (LSB)      |
    // +--------+--------+--------+--------+
    //
    // Example:  12345
    //
    // Digits:  0   1   2   3   4   5
    // Nibbles: 0 | 1 | 2 | 4 | 4 | 5
    //
    // Bytes:
    // +--------+--------+--------+
    // |  0x01  |  0x23  |  0x45  |
    // +--------+--------+--------+
    //
    // Bit layout (one byte):
    //   bit7 bit6 bit5 bit4 | bit3 bit2 bit1 bit0
    //   --------------------+--------------------
    //       High Nibble     |      Low Nibble
    //
    // Rules:
    // - Each digit occupies one nibble (0x0 – 0x9)
    // - Total bytes = ceil(nibbles / 2)
    //

    public static int GetByteLength(int digitCount)
    {
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(digitCount);

        return (digitCount + 1) / 2; // ceil(nibbles / 2)
    }

    private static readonly UIntMapper _UIntMapper = new();

    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException("Decimal digits not supported in COMP-6");
        
        if (pic.Signed)
            throw new NotSupportedException("Signed value is not valid for COMP-6");

        // Decode BCD
        byte[] chars = DecodeUPacked(buffer, pic.DigitCount); // 根據 PIC 長度解碼 BCD

        decimal value = CbDecimal.Decode(chars, pic.DecimalDigits, isNegative: false);

        return _UIntMapper.Map(value, pic);
    }

    public static byte[] Encode(NumericMeta nMeta, PicMeta pic)
    {                
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException("Decimal digits not supported in COMP-6");

        if (pic.Signed)
            throw new NotSupportedException("Signed value is not valid for COMP-6");

        if (nMeta.IsNegative)
            throw new ArgumentException("Negative value is not valid for COMP-6", nameof(nMeta));

        int byteLen = COMP3.GetByteLength(pic.DigitCount); // 共用計算公式
        byte[] buffer = new byte[byteLen];

        ReadOnlySpan<byte> digits = nMeta.Chars;

        int digitIndex = digits.Length - 1;
        int byteIndex  = buffer.Length - 1;

        while (byteIndex >= 0)
        {
            int low  = digitIndex >= 0 ? digits[digitIndex--] - (byte)'0' : 0;
            int high = digitIndex >= 0 ? digits[digitIndex--] - (byte)'0' : 0;
            buffer[byteIndex--] = (byte)((high << 4) | low);
        }

        return buffer;
    }

    private static byte[] DecodeUPacked(ReadOnlySpan<byte> buffer, int digits)
    {
        if (digits < 1)
            throw new ArgumentOutOfRangeException(nameof(digits), "Digits must be greater than 0.");
        
        byte[] bytes = new byte[digits];

        int outIndex = digits - 1;
        int byteIndex = buffer.Length - 1;

        int remaining = digits;

        while (remaining > 0)
        {
            byte b = buffer[byteIndex--];

            // low nibble
            bytes[outIndex--] = (byte)('0' + (b & 0x0F));
            remaining--;

            if (remaining > 0)
            {
                // high nibble
                bytes[outIndex--] = (byte)('0' + ((b >> 4) & 0x0F));
                remaining--;
            }
        }

        return bytes;
    }
}
