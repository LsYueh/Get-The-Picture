using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Encoder.Category;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP-3 (Packed Decimal)
/// </summary>
internal static class COMP3
{
    private const int POSITIVE_SIGN = 0x0C; // TODO: 考慮是否要根據 DataStorageOptions 去切換 NibbleCodex
    private const int NEGATIVE_SIGN = 0x0D;
    private const int UNSIGNED      = 0x0F;

    // Packed Decimal (COMP-3) Bit / Nibble Format
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
    // Example:  -12345
    //
    // Digits:  1   2   3   4   5   Sign
    // Nibbles: 1 | 2 | 3 | 4 | 5 |  D
    //
    // Bytes:
    // +--------+--------+--------+
    // |  0x12  |  0x34  |  0x5D  |
    // +--------+--------+--------+
    //
    // Bit layout (one byte):
    //   bit7 bit6 bit5 bit4 | bit3 bit2 bit1 bit0
    //   --------------------+--------------------
    //       High Nibble     |      Low Nibble
    //
    // Rules:
    // - Each digit occupies one nibble (0x0 – 0x9)
    // - Last nibble is the sign
    //     C = positive
    //     D = negative
    //     F = unsigned / positive (vendor dependent)
    // - Total bytes = (number_of_digits + 1) / 2
    //

    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, DataStorageOptions ds = DataStorageOptions.CI)
    {
        byte[] chars = DecodePacked(buffer, pic.DigitCount, out bool isNegative); // 根據 PIC 長度解碼 BCD

        if (pic.DecimalDigits > 0)
            return CbDecimal.Decode(chars, pic.DecimalDigits, isNegative);
        
        if (!pic.Signed)
        {
            if (isNegative)
                throw new OverflowException("Unsigned field contains negative number");
                
            return DecodeUInt64(chars);
        }

        return DecodeInt64(chars, isNegative);
    }

    public static byte[] Encode(NumericMeta nMeta, PicMeta pic, DataStorageOptions ds = DataStorageOptions.CI)
    {        
        if (!pic.Signed && nMeta.IsNegative)
            throw new InvalidOperationException("Unsigned PIC cannot encode negative value");

        int byteLen = (pic.DigitCount + 1) / 2;
        byte[] buffer = new byte[byteLen];

        ReadOnlySpan<byte> digits = nMeta.Chars;

        int digitIndex = digits.Length - 1;
        int byteIndex  = buffer.Length - 1;

        // LSB : digit + sign
        int low  = (!pic.Signed) ? UNSIGNED : (nMeta.IsNegative ? NEGATIVE_SIGN : POSITIVE_SIGN);
        int high = digitIndex >= 0 ? digits[digitIndex--] - (byte)'0' : 0;

        buffer[byteIndex--] = (byte)((high << 4) | low);

        while (byteIndex >= 0)
        {
            low  = digitIndex >= 0 ? digits[digitIndex--] - (byte)'0' : 0;
            high = digitIndex >= 0 ? digits[digitIndex--] - (byte)'0' : 0;
            buffer[byteIndex--] = (byte)((high << 4) | low);
        }

        return buffer;
    }

    private static byte[] DecodePacked(ReadOnlySpan<byte> buffer, int digits, out bool negative)
    {
        byte[] bytes = new byte[digits];

        int idx = digits - 1;
        negative = false;

        int LSB = buffer.Length - 1;

        for (int i = LSB; i >= 0; i--)
        {
            byte b = buffer[i];
            int low  =  b       & 0x0F; // Bit Mask
            int high = (b >> 4) & 0x0F; // Bit Mask

            if (i == LSB)
            {
                negative = low switch
                {
                    NEGATIVE_SIGN => true,
                    POSITIVE_SIGN or UNSIGNED => false,
                    _ => throw new FormatException($"Invalid COMP-3 sign nibble: {low:X}")
                };

                if (idx >= 0)
                    bytes[idx--] = (byte)('0' + high);
            }
            else
            {
                if (idx >= 0)
                    bytes[idx--] = (byte)('0' + low);

                if (idx >= 0)
                    bytes[idx--] = (byte)('0' + high);
            }
        }

        return bytes;
    }

    private static long DecodeInt64(ReadOnlySpan<byte> chars, bool isNegative)
    {
        if (chars.Length > 18)
            throw new OverflowException("Packed number too large for Int64");

        long value = 0;
        foreach (byte c in chars)
            value = value * 10 + (c - '0');

        return isNegative ? -value : value;
    }

    private static ulong DecodeUInt64(ReadOnlySpan<byte> chars)
    {
        ulong value = 0;

        foreach (byte c in chars)
        {
            ulong digit = (ulong)(c - '0');

            // overflow check
            if (value > (ulong.MaxValue - digit) / 10)
                throw new OverflowException("Packed number too large for UInt64");

            value = value * 10 + digit;
        }

        return value;
    }
}
