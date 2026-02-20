using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Base.Overpunch.Codex;

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
    /// <param name="isNegative"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static Span<byte> Decode(Span<byte> bytes, PicMeta pic, CodecOptions options, out bool isNegative)
    {
        isNegative = false;

        if (pic.Signed)
        {
            Index index = options.Sign switch
            {
                SignOptions.IsTrailing => ^1,
                SignOptions.IsLeading  => 0,
                _ => throw new FormatException($"Unsupported Sign option: {options.Sign}")
            };

            byte key = (byte)(bytes[index] & 0x7F); // ASCII overpunch
            string opVal = OpCodex.TryGetValue(key, options.DataStorage);

            char sign = opVal[0];
            isNegative = sign switch
            {
                '+' => false,
                '-' => true,
                _   => throw new FormatException($"Invalid overpunch sign: '{sign}'"),
            };

            bytes[index] = (byte)opVal[1];
        }

        EnsureAllAsciiDigits(bytes);

        return bytes; // 數字文 (char[])
    }

    /// <summary>
    /// 符號(sign)與數字文(numeric) → PIC 9/S9
    /// </summary>
    /// <param name="isNegative"></param>
    /// <param name="numeric">數字文 (char[])</param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    public static byte[] Encode(bool isNegative, byte[] numeric, PicMeta pic, CodecOptions options)
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

            char digit = (char)(numeric[index] & 0x7F); // ASCII overpunch
            string opValue = OpCodexBase.OpVal(isNegative, digit);

            byte value = OpCodex.TryGetKey(opValue, options.DataStorage);

            numeric[index] = value;
        }

        return numeric;
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