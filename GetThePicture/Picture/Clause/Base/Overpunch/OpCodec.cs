using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Base.Overpunch;

/// <summary>
/// Overpunch Codec
/// </summary>
public static class OpCodec
{
    /// <summary>
    /// PIC 9/S9 → 符號(sign)與數字文(numeric)
    /// </summary>
    /// <param name="bytes"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <param name="sign">符號</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static Span<byte> Decode(Span<byte> bytes, PicMeta pic, CodecOptions options, out decimal sign)
    {
        sign = 1.0m;

        if (pic.Signed)
        {
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            byte key = (byte)(bytes[index] & 0x7F); // ASCII overpunch

            OpVal opVal = GetOpValue(key, options.DataStorage);

            bytes[index] = opVal.Digit;
            sign = opVal.Sign;
        }

        EnsureAllAsciiDigits(bytes);

        return bytes; // 數字文 (char[])
    }

    /// <summary>
    /// 符號(sign)與數字文(numeric) → PIC 9/S9
    /// </summary>
    /// <param name="sign">符號</param>
    /// <param name="numeric">數字文 (char[])</param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    public static byte[] Encode(decimal sign, byte[] numeric, PicMeta pic, CodecOptions options)
    {
        EnsureAllAsciiDigits(numeric);

        if (pic.Signed)
        {
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            byte digit = (byte)(numeric[index] & 0x7F); // ASCII overpunch

            byte value = GetOpKey(new OpVal(sign, digit), options.DataStorage);

            numeric[index] = value;
        }

        return numeric;
    }

    /// <summary>
    /// Get Overpunch Value
    /// </summary>
    /// <param name="key"></param>
    /// <param name="ds">DataStorage Options</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    private static OpVal GetOpValue(byte key, DataStorageOptions ds)
    {
        if (!OpCodex.Map.TryGetValue(ds, out Dictionary<byte, OpVal>? codex))
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
    private static byte GetOpKey(OpVal value, DataStorageOptions ds)
    {
        if (!OpCodex.ReversedMap.TryGetValue(ds, out Dictionary<OpVal, byte>? codex))
            throw new FormatException($"Unsupported DataStorage: {ds}");

        if (!codex.TryGetValue(value, out byte key))
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