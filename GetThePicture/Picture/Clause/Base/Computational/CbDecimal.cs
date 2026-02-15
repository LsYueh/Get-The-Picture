namespace GetThePicture.Picture.Clause.Base.Computational;

public class CbDecimal
{
    /// <summary>
    /// Decode from byte span (e.g., COMP-3 / overpunch digits in ASCII).
    /// </summary>
    /// <param name="bytes">The integer digits (PIC 9(n)) as chars.</param>
    /// <param name="decimalDigits">Number of decimal digits (V9(m)).</param>
    /// <param name="isNegative">Sign flag.</param>
    /// <returns>Decoded decimal value.</returns>
    /// <exception cref="FormatException">If any char is not 0–9.</exception>
    /// <exception cref="OverflowException">If total digits exceed .NET decimal precision.</exception>
    public static decimal Decode(ReadOnlySpan<byte> chars, int decimalDigits, bool isNegative)
    {
        decimal result;

        if (chars.Length <= 18)
        {
            // long fast-path（安全 18 位）
            long value = 0;

            foreach (byte c in chars)
            {
                int digit = c - (byte)'0';
                if ((uint)digit > 9)
                    throw new FormatException($"Invalid digit '{(char)c}' in numeric field.");

                checked
                {
                    value = value * 10 + digit;
                }
            }

            result = value;
        }
        else
        {
            // decimal fallback
            decimal value = 0m;

            foreach (byte c in chars)
            {
                int digit = c - (byte)'0';
                if ((uint)digit > 9)
                    throw new FormatException($"Invalid digit '{(char)c}' in numeric field.");

                value = value * 10m + digit;
            }

            result = value;
        }

        // scale adjustment
        if (decimalDigits > 0)
            result /= Pow10(decimalDigits);

            
        if (isNegative)
            result = -result;

        return result;
    }

    /// <summary>
    /// Pow10 lookup table for 0..28 decimal digits.
    /// </summary>
    public static decimal Pow10(int n)
    {
        if (n >= Pow10Table.Length)
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
