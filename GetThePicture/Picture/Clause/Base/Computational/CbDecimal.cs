namespace GetThePicture.Picture.Clause.Base.Computational;

public class CbDecimal
{
    /// <summary>
    /// Decode from byte span (e.g., COMP-3 / overpunch digits in ASCII).
    /// </summary>
    /// <param name="chars">The integer digits (PIC 9(n)) as chars.</param>
    /// <param name="decimalDigits">Number of decimal digits (V9(m)).</param>
    /// <param name="isNegative">Sign flag.</param>
    /// <returns>Decoded decimal value.</returns>
    /// <exception cref="FormatException">If any char is not 0–9.</exception>
    /// <exception cref="OverflowException">If total digits exceed .NET decimal precision.</exception>
    public static decimal Decode(ReadOnlySpan<char> chars, int decimalDigits, bool isNegative)
    {
        int totalDigits = chars.Length;

        if (chars.Length > 28)
            throw new OverflowException($"Total digits ({totalDigits}) exceed .NET decimal precision (28 digits max).");

        decimal value = 0m;

        foreach (char c in chars)
        {
            int digit = c - '0';

            if ((uint)digit > 9)
                throw new FormatException($"Invalid digit '{c}' in numeric field.");

            value = value * 10m + digit;
        }

        if (decimalDigits > 0)
        {
            value /= Pow10(decimalDigits);
        }

        return isNegative ? -value : value;
    }

    /// <summary>
    /// Decode from bytes span (e.g., DISPLAY digits).
    /// </summary>
    public static decimal Decode(ReadOnlySpan<byte> bytes, int decimalDigits, bool isNegative)
    {
        int totalDigits = bytes.Length;

        if (totalDigits > 28)
            throw new OverflowException($"Total digits ({totalDigits}) exceed .NET decimal precision (28 digits max).");

        decimal value = 0m;

        foreach (byte b in bytes)
        {
            int digit = b - (byte)'0';

            if ((uint)digit > 9)
                throw new FormatException($"Invalid digit '{(char)b}' in numeric field.");

            value = value * 10m + digit;
        }

        if (decimalDigits > 0)
        {
            value /= Pow10(decimalDigits);
        }

        return isNegative ? -value : value;
    }

    /// <summary>
    /// Pow10 lookup table for 0..28 decimal digits.
    /// </summary>
    private static decimal Pow10(int n)
    {
        if ((uint)n >= Pow10Table.Length)
            throw new OverflowException("Decimal scale too large.");

        return Pow10Table[n];
    }

    private static readonly decimal[] Pow10Table =
    [
        1m,                              // 10^0
        10m,                             // 10^1
        100m,
        1000m,
        10000m,
        100000m,
        1000000m,
        10000000m,
        100000000m,
        1000000000m,
        10000000000m,
        100000000000m,
        1000000000000m,
        10000000000000m,
        100000000000000m,
        1000000000000000m,
        10000000000000000m,
        100000000000000000m,
        1000000000000000000m,
        10000000000000000000m,
        100000000000000000000m,
        1000000000000000000000m,
        10000000000000000000000m,
        100000000000000000000000m,
        1000000000000000000000000m,
        10000000000000000000000000m,
        100000000000000000000000000m,
        1000000000000000000000000000m,
        10000000000000000000000000000m  // 10^28
    ];
}
