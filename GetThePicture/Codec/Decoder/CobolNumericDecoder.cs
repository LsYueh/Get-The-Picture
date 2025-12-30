using System.Globalization;

using GetThePicture.Cobol;

namespace GetThePicture.Codec.Decoder;

internal static class CobolNumericDecoder
{
    public static object Decode(string display, PicClause pic)
    {
        string work = display;
        bool negative = false;

        // S9 handling (leading / trailing sign)
        if (pic.Signed)
        {
            char first = work[0];
            char last  = work[^1];

            if (first == '+' || first == '-')
            {
                negative = first == '-';
                work = work.Substring(1);
            }
            else if (last == '+' || last == '-')
            {
                negative = last == '-';
                work = work.Substring(0, work.Length - 1);
            }
        }

        if (!work.All(char.IsDigit))
        {
            throw new FormatException($"Invalid numeric DISPLAY value: '{display}'");
        }

        int intDigits = pic.IntegerDigits;
        int decDigits = pic.DecimalDigits;

        if (decDigits == 0)
        {
            if (!long.TryParse(work, NumberStyles.None, CultureInfo.InvariantCulture, out long value))
                throw new FormatException($"Invalid integer DISPLAY value: '{display}'");

            return negative ? -value : value;
        }
        else
        {
            if (work.Length != intDigits + decDigits)
            {
                throw new FormatException(
                    $"Numeric length mismatch for PIC. Expected {intDigits + decDigits}, actual {work.Length}.");
            }

            string withDot = work.Insert(intDigits, ".");

            if (!decimal.TryParse(withDot, NumberStyles.None, CultureInfo.InvariantCulture, out decimal value))
                throw new FormatException($"Invalid decimal DISPLAY value: '{display}'");

            return negative ? -value : value;
        }
    }
}
