using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;
using GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

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

    private static readonly SIntMapper _SIntMapper = new();
    private static readonly UIntMapper _UIntMapper = new();

    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, DataStorageOptions ds = DataStorageOptions.CI)
    {
        // Decode BCD
        byte[] chars = DecodePacked(buffer, pic.DigitCount, out bool isNegative); // 根據 PIC 長度解碼 BCD

        if (!pic.Signed && isNegative)
            throw new OverflowException("Unsigned field contains negative number");

        decimal value = CbDecimal.Decode(chars, pic.DecimalDigits, isNegative);

        if (pic.DecimalDigits > 0)
            return value;

        IMapper mapper = pic.Signed ? _SIntMapper : _UIntMapper;

        return mapper.Map(value, pic);
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
        if (digits < 1)
            throw new ArgumentOutOfRangeException(nameof(digits), "Digits must be greater than 0.");
        
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
}
