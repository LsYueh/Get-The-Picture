using System.Text;

using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Cobol.Picture.OverpunchBase;

public static class Overpunch
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
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            char key = (char)(fieldBytes[index] & 0x7F); // ASCII overpunch

            OpVal opVal = GetOpValue(key, options.DataStorage);

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

        EnsureAllAsciiDigits(buffer);

        if (pic.Signed)
        {
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            char digit = (char)(buffer[index] & 0x7F); // ASCII overpunch

            char value = GetOpKey(new OpVal(sign, digit), options.DataStorage);

            buffer[index] = (byte) value;
        }

        return buffer;
    }

    /// <summary>
    /// Get Overpunch Value
    /// </summary>
    /// <param name="key"></param>
    /// <param name="ds">DataStorage Options</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    private static OpVal GetOpValue(char key, DataStorageOptions ds)
    {
        if (!OverpunchCodex.Map.TryGetValue(ds, out Dictionary<char, OpVal>? codex))
            throw new FormatException($"Unsupported DataStorage: {ds}");

        if (!codex.TryGetValue(key, out OpVal value))
            throw new FormatException($"Invalid overpunch search key: '{key}'");

        return value;
    }

    /// <summary>
    /// Get Overpunch Key
    /// </summary>
    /// <param name="value"></param>
    /// <param name="ds">DataStorage Options</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    private static char GetOpKey(OpVal value, DataStorageOptions ds)
    {
        if (!OverpunchCodex.ReversedMap.TryGetValue(ds, out Dictionary<OpVal, char>? codex))
            throw new FormatException($"Unsupported DataStorage: {ds}");

        if (!codex.TryGetValue(value, out char key))
            throw new FormatException($"Invalid overpunch search value: '{value}'");

        return key;
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