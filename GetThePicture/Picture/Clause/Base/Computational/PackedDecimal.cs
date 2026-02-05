using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Encoder.Meta;

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
        PackedNumber pn = DecodePacked(buffer, pic.DigitCount); // 根據 PIC 長度解碼 BCD

        if (pic.DecimalDigits > 0)
            return DecodeDecimal(pn, pic);
        
        if (!pic.Signed)
        {
            if (pn.IsNegative)
                throw new OverflowException("Unsigned field contains negative number");
                
            return DecodeUInt64(pn);
        }

        return DecodeInt64(pn);
    }

    public static byte[] Encode(CobMeta meta, PicMeta pic, DataStorageOptions ds = DataStorageOptions.CI)
    {
        if (meta.Number is null)
            throw new ArgumentNullException(nameof(meta));

        var number = meta.Number.Value;

        if (!pic.Signed && number.IsNegative)
            throw new InvalidOperationException("Unsigned PIC cannot encode negative value");

        string digits = number.Digits;

        int byteLen = (pic.DigitCount + 1) / 2;
        byte[] buffer = new byte[byteLen];

        int digitIndex = digits.Length - 1;
        int byteIndex  = buffer.Length - 1;

        // LSB : digit + sign
        int low  = (!pic.Signed) ? UNSIGNED : (number.IsNegative ? NEGATIVE_SIGN : POSITIVE_SIGN);
        int high = digitIndex >= 0 ? digits[digitIndex--] - '0' : 0;

        buffer[byteIndex--] = (byte)((high << 4) | low);

        while (byteIndex >= 0)
        {
            low  = digitIndex >= 0 ? digits[digitIndex--] - '0' : 0;
            high = digitIndex >= 0 ? digits[digitIndex--] - '0' : 0;
            buffer[byteIndex--] = (byte)((high << 4) | low);
        }

        return buffer;
    }

    private readonly struct PackedNumber
    {
        public PackedNumber(ReadOnlySpan<char> digits, bool negative)
        {
            Digits = digits.ToString();
            IsNegative = negative;
        }

        public string Digits { get; }
        public bool IsNegative { get; }
    }

    private static PackedNumber DecodePacked(ReadOnlySpan<byte> buffer, int digits)
    {
        Span<char> chars = stackalloc char[digits];

        int idx = digits - 1;
        bool negative = false;

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
                    chars[idx--] = (char)('0' + high);
            }
            else
            {
                if (idx >= 0)
                    chars[idx--] = (char)('0' + low);
                if (idx >= 0)
                    chars[idx--] = (char)('0' + high);
            }
        }

        return new PackedNumber(chars, negative);
    }

    private static long DecodeInt64(PackedNumber pn)
    {
        if (pn.Digits.Length > 18)
            throw new OverflowException("Packed number too large for Int64");

        long value = 0;
        foreach (char c in pn.Digits)
            value = value * 10 + (c - '0');

        return pn.IsNegative ? -value : value;
    }

    private static ulong DecodeUInt64(PackedNumber pn)
    {
        ulong value = 0;

        foreach (char c in pn.Digits)
        {
            ulong digit = (ulong)(c - '0');

            // overflow check
            if (value > (ulong.MaxValue - digit) / 10)
                throw new OverflowException("Packed number too large for UInt64");

            value = value * 10 + digit;
        }

        return value;
    }


    private static decimal DecodeDecimal(PackedNumber pn, PicMeta pic)
    {
        decimal value = 0m;
        foreach (char c in pn.Digits)
            value = value * 10m + (c - '0');

        decimal scale = 1m;
        for (int i = 0; i < pic.DecimalDigits; i++)
            scale *= 10m;

        value /= scale;

        return pn.IsNegative ? -value : value;
    }
}
