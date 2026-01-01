using System.Text;
using System.Globalization;

using GetThePicture.Cobol;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Decoder;

internal static class CobolNumericDecoder
{
    /// <summary>
    /// Decoder for COBOL PIC 9/S9.
    /// </summary>
    /// <param name="cp950Bytes"></param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(byte[] cp950Bytes, PicClause pic, CodecOptions? options = null)
    {
        Encoding cp950 = EncodingFactory.CP950;

        options ??= new CodecOptions();

        // 根據PIC內容限制大小
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(cp950Bytes, 0, pic.TotalLength);

        Span<byte> span = stackalloc byte[fieldBytes.Length];
        fieldBytes.CopyTo(span);

        int sign = 1;

        // Overpunch decode
        if (pic.Signed)
        {
            OpVal opVal = GetOverpunchValue(fieldBytes, options);

            // 用 Overpunch 對應值「寫到 scratch」
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            span[index] = (byte) opVal.Digit;
            sign = opVal.Sign;
        }
        
        EnsureAllAsciiDigits(span);
        
        var display = cp950.GetString(span);

        object value = ParseValue(display, pic);

#pragma warning disable IDE0066 // Convert switch statement to expression
        switch (value)
        {
            case long    l: return sign * l; // l 保持 long
            case decimal d: return sign * d; // d 保持 decimal
            default:
                throw new InvalidOperationException("Unsupported numeric type");
        }
#pragma warning restore IDE0066 // Convert switch statement to expression
    }

    private static OpVal GetOverpunchValue(ReadOnlySpan<byte> fieldBytes, CodecOptions options)
    {
        if (fieldBytes.IsEmpty)
            throw new FormatException("Field bytes is empty.");

        if (!OverpunchCode.Map.TryGetValue(options.DataStorage, out Dictionary<char, OpVal>? codex))
            throw new FormatException($"Unsupported DataStorage: {options.DataStorage}");

        Index index = options.Sign switch
        {
            SignOptions.IsTrailing => ^1,
            SignOptions.IsLeading  => 0,
            _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
        };

        char key = (char)(fieldBytes[index] & 0x7F); // ASCII overpunch

        if (!codex.TryGetValue(key, out OpVal value))
            throw new FormatException($"Invalid overpunch digit: '{key}'");

        return value;
    }

    private static void EnsureAllAsciiDigits(ReadOnlySpan<byte> span)
    {
        for (int i = 0; i < span.Length; i++)
        {
            if ((uint)(span[i] - (byte)'0') > 9)
                throw new FormatException($"Invalid digit at index {i+1}"); // Note: 轉成 1-based
        }
    }

    private static object ParseValue(string display, PicClause pic)
    {
        if (string.IsNullOrEmpty(display))
            throw new FormatException("DISPLAY value is empty.");

        if (pic.DecimalDigits == 0)
        {
            return ParseInteger(display);
        }
        else
        {
            return  ParseDecimal(display, pic);
        }
    }

    private static long ParseInteger(string display)
    {
        if (!long.TryParse(
                display,
                NumberStyles.None,
                CultureInfo.InvariantCulture,
                out long value))
        {
            throw new FormatException($"Invalid integer DISPLAY value: '{display}'");
        }

        return value;
    }

    private static decimal ParseDecimal(string display, PicClause pic)
    {
        int expectedLength = pic.IntegerDigits + pic.DecimalDigits;

        if (display.Length != expectedLength)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {expectedLength}, actual {display.Length}.");

        // 插入小數點
        string withDot = (pic.DecimalDigits > 0) ? display.Insert(pic.IntegerDigits, ".") : display;

        if (!decimal.TryParse(
                withDot,
                NumberStyles.AllowDecimalPoint,
                CultureInfo.InvariantCulture,
                out decimal value))
        {
            throw new FormatException($"Invalid decimal DISPLAY value: '{withDot} ({display})'");
        }

        return value;
    }
}
