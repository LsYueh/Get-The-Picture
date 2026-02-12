using static GetThePicture.Picture.Clause.Encoder.Category.NumericEncoder;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP-6 (Unsigned Packed Decimal)
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
    // - Total bytes = (number_of_digits + 1) / 2
    //

    public static ulong Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException("Decimal digits not supported in COMP-6");
        
        if (pic.Signed)
            throw new NotSupportedException("Signed value is not valid for COMP-6");

        Span<char> chars = stackalloc char[pic.DigitCount]; // 根據 PIC 長度解碼 BCD

        DecodeUPacked(buffer, chars); // 解析 COMP-6 → chars

        return DecodeUInt64(chars); // chars → ulong
    }

    public static byte[] Encode(NumericValue nValue, PicMeta pic)
    {                
        if (pic.DecimalDigits > 0)
            throw new NotSupportedException("Decimal digits not supported in COMP-6");

        if (pic.Signed)
            throw new NotSupportedException("Signed value is not valid for COMP-6");

        if (nValue.IsNegative)
            throw new ArgumentException("Negative value is not valid for COMP-6", nameof(nValue));

        int byteLen = (pic.DigitCount + 1) / 2;
        byte[] buffer = new byte[byteLen];

        ReadOnlySpan<byte> digits = nValue.Magnitude.Span;

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

    private static void DecodeUPacked(ReadOnlySpan<byte> buffer, Span<char> output)
    {
        int digits = output.Length;

        int expectedLength = (digits + 1) / 2;
        if (buffer.Length != expectedLength)
            throw new ArgumentOutOfRangeException(nameof(buffer), buffer.Length,
                $"Buffer length {buffer.Length} does not match expected byte count {expectedLength} for {digits} digits.");
        
        int idx = digits - 1;

        for (int i = buffer.Length - 1; i >= 0; i--)
        {
            byte b = buffer[i];
            int low  =  b       & 0x0F; // Bit Mask
            int high = (b >> 4) & 0x0F; // Bit Mask

            if (low > 9 || high > 9)
                throw new FormatException("Invalid COMP-6 packed digit.");

            if (idx >= 0)
                output[idx--] = (char)('0' + low);
            if (idx >= 0)
                output[idx--] = (char)('0' + high);
        }
    }

    private static ulong DecodeUInt64(ReadOnlySpan<char> chars)
    {
        ulong value = 0;

        foreach (char c in chars)
        {
            ulong digit = (ulong)(c - '0');
            
            if (value > (ulong.MaxValue - digit) / 10)
                throw new OverflowException("Packed number too large for UInt64");

            value = value * 10 + digit;
        }

        return value;
    }
}
