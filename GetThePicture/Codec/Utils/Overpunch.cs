using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;

namespace GetThePicture.Codec.Utils;

internal static class Overpunch
{
    /// <summary>
    /// PIC 9/S9 → 符號(sign)與數字文(numeric)
    /// </summary>
    /// <param name="fieldBytes"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <param name="sign">符號</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static string Decode(ReadOnlySpan<byte> fieldBytes, PicClause pic, CodecOptions options, out decimal sign)
    {
        byte[] buffer = new byte[fieldBytes.Length];
        fieldBytes.CopyTo(buffer);

        sign = 1.0m;

        if (pic.Signed)
        {
            OpVal opVal = GetOverpunchValue(fieldBytes, options);

            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            buffer[index] = (byte) opVal.Digit;
            sign = opVal.Sign;
        }

        EnsureAllAsciiDigits(buffer);

        Encoding cp950 = EncodingFactory.CP950;
        string numeric = cp950.GetString(buffer); // 數字文

        return numeric;
    }

    /// <summary>
    /// 符號(sign)與數字文(numeric) → PIC 9/S9
    /// </summary>
    /// <param name="sign">符號</param>
    /// <param name="numeric">數字文</param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    public static byte[] Encode(decimal sign, string numeric, PicClause pic, CodecOptions options)
    {
        Encoding cp950 = EncodingFactory.CP950;

        byte[] buffer = cp950.GetBytes(numeric);

        if (!pic.Signed)
        {
            return buffer;
        }

        // TODO: 查表替換...

        return buffer;
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
}