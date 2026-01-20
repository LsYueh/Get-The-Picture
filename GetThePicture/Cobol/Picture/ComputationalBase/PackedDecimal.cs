using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Cobol.Picture.ComputationalBase;

/// <summary>
/// COMP-3 (Packed Decimal)
/// </summary>
internal static class COMP3
{
    public static object Decode(ReadOnlySpan<byte> buffer, PicClause pic, DataStorageOptions ds = DataStorageOptions.CI)
    {
        PackedNumber pn = DecodePacked(buffer, pic.DigitCount);

        if (pic.DecimalDigits > 0)
            return DecodeDecimal(pn, pic);

        if (!pic.Signed)
            return DecodeUInt64(pn);

        return DecodeInt64(pn);
    }

    public static byte[] Encode(DisplayValue displayValue, PicClause pic, DataStorageOptions ds = DataStorageOptions.CI)
    {
        if (displayValue.Number is null)
            throw new ArgumentNullException(nameof(displayValue));

        var number = displayValue.Number.Value;

        if (!pic.Signed && number.IsNegative)
            throw new InvalidOperationException("Unsigned PIC cannot encode negative value");

        string digits = number.Digits;

        if (digits.Length > pic.DigitCount)
            throw new FormatException("Too many digits for PIC");

        int byteLen = (pic.DigitCount + 1) / 2;
        byte[] buffer = new byte[byteLen];

        int digitIndex = digits.Length - 1;
        int byteIndex  = buffer.Length - 1;

        // last byte: digit + sign
        int low  = (!pic.Signed) ? 0x0F : (number.IsNegative ? 0x0D : 0x0C);
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

        for (int i = buffer.Length - 1; i >= 0; i--)
        {
            byte b = buffer[i];
            int low  = b & 0x0F;
            int high = (b >> 4) & 0x0F;

            if (i == buffer.Length - 1)
            {
                negative = low switch
                {
                    0x0D => true,
                    0x0C or 0x0F => false,
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


    private static decimal DecodeDecimal(PackedNumber pn, PicClause pic)
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
