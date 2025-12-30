using System.Text.RegularExpressions;

namespace GetThePicture.Pic;

public static partial class Pic
{
    [GeneratedRegex(@"^X(\((\d+)\))?$", RegexOptions.IgnoreCase)]
    private static partial Regex XRegex();

    [GeneratedRegex(@"^(S)?((9(\(\d+\))?)|9+)(V((9(\(\d+\))?)|9+))?$", RegexOptions.IgnoreCase)]
    private static partial Regex NumericRegex();
            
    public static PicClause Parse(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            throw new ArgumentException("PIC clause is empty.");
        
        input = input.ToUpperInvariant().Replace(" ", string.Empty);

        // ─────────────────────────
        // Alphanumeric
        // ─────────────────────────
        var xMatch = XRegex().Match(input);
        if (xMatch.Success)
        {
            int len = xMatch.Groups[2].Success ? int.Parse(xMatch.Groups[2].Value) : 1;

            return new PicClause
            {
                DataType = PicDataType.Alphanumeric,
                Signed = false,
                IntegerDigits = len,
                DecimalDigits = 0
            };
        }

        // ─────────────────────────
        // Numeric
        // ─────────────────────────
        var numMatch = NumericRegex().Match(input);
        if (numMatch.Success)
        {
            bool signed = numMatch.Groups[1].Success;

            int intDigits = CountDigits(numMatch.Groups[2].Value);
            int decDigits = numMatch.Groups[6].Success ? CountDigits(numMatch.Groups[6].Value) : 0;

            return new PicClause
            {
                DataType = PicDataType.Numeric,
                Signed = signed,
                IntegerDigits = intDigits,
                DecimalDigits = decDigits
            };
        }

        throw new NotSupportedException($"Unsupported PIC clause: {input}");
    }

    [GeneratedRegex(@"9\((\d+)\)")]
    private static partial Regex _9();

    private static int CountDigits(string token)
    {
        // 9(5)
        var m = _9().Match(token);
        if (m.Success)
            return int.Parse(m.Groups[1].Value);

        // 9999
        int count = 0;
        foreach (char c in token)
            if (c == '9') count++;

        return count;
    }
}
