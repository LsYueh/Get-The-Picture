using System.Collections.ObjectModel;
using System.Text;

using GetThePicture.Cobol.Picture.OverpunchBase;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Cobol.Picture;

public enum DataStorageOptions
{
  /// <summary>RM/COBOL (not RM/COBOL-85)</summary>
  CA,
  /// <summary>MBP COBOL</summary>
  CB,
  /// <summary>IBM COBOL (RM/COBOL-85)</summary>
  CI,
  /// <summary>Micro Focus COBOL</summary>
  CM,
  /// <summary>NCR COBOL</summary>
  CN,
  /// <summary>Realia COBOL</summary>
  CR,
  /// <summary>VAX COBOL</summary>
  CV,
}

/// <summary>
/// `SIGN IS TRAILING` is the default.
/// </summary>
public enum SignOptions {
  IsTrailing, // default
  IsLeading,
}

/// <summary>
/// 
/// </summary>
internal static class OverpunchCodex
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<char, OpVal>> Map;
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, char>> ReversedMap;

    static OverpunchCodex()
    {
        Map = BuildMap();
        ReversedMap = BuildReversedMap();
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<char, OpVal>> BuildMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<char, OpVal>>
        {
            { DataStorageOptions.CA, Merge(OpCodex.OP_POSITIVE_01, OpCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CB, Merge(OpCodex.OP_POSITIVE_01, OpCodex.OP_NEGATIVE_02) },
            { DataStorageOptions.CI, Merge(OpCodex.OP_POSITIVE_02, OpCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CM, Merge(OpCodex.OP_POSITIVE_01, OpCodex.OP_NEGATIVE_03) },
            { DataStorageOptions.CN, Merge(OpCodex.OP_POSITIVE_02, OpCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CR, Merge(OpCodex.OP_POSITIVE_01, OpCodex.OP_NEGATIVE_04) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<char, OpVal>>(dictionary);
    }

    private static Dictionary<char, OpVal> Merge(
        Dictionary<char, OpVal> a,
        Dictionary<char, OpVal> b)
    {
        var dict = new Dictionary<char, OpVal>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, char>> BuildReversedMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<OpVal, char>>
        {
            { DataStorageOptions.CA, MergeRev(OpCodex.OP_POSITIVE_01_REVERSE, OpCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CB, MergeRev(OpCodex.OP_POSITIVE_01_REVERSE, OpCodex.OP_NEGATIVE_02_REVERSE) },
            { DataStorageOptions.CI, MergeRev(OpCodex.OP_POSITIVE_02_REVERSE, OpCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CM, MergeRev(OpCodex.OP_POSITIVE_01_REVERSE, OpCodex.OP_NEGATIVE_03_REVERSE) },
            { DataStorageOptions.CN, MergeRev(OpCodex.OP_POSITIVE_02_REVERSE, OpCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CR, MergeRev(OpCodex.OP_POSITIVE_01_REVERSE, OpCodex.OP_NEGATIVE_04_REVERSE) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, char>>(dictionary);
    }

    private static Dictionary<OpVal, char> MergeRev(
        Dictionary<OpVal, char> a,
        Dictionary<OpVal, char> b)
    {
        var dict = new Dictionary<OpVal, char>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }
}

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

            OpVal opVal = GetOpValue(key, options);

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

            char value = GetOpKey(new OpVal(sign, digit), options);

            buffer[index] = (byte) value;
        }

        return buffer;
    }

    /// <summary>
    /// Get Overpunch Value
    /// </summary>
    /// <param name="key"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    private static OpVal GetOpValue(char key, CodecOptions options)
    {
        if (!OverpunchCodex.Map.TryGetValue(options.DataStorage, out Dictionary<char, OpVal>? codex))
            throw new FormatException($"Unsupported DataStorage: {options.DataStorage}");

        if (!codex.TryGetValue(key, out OpVal value))
            throw new FormatException($"Invalid overpunch search key: '{key}'");

        return value;
    }

    /// <summary>
    /// Get Overpunch Key
    /// </summary>
    /// <param name="value"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    private static char GetOpKey(OpVal value, CodecOptions options)
    {
        if (!OverpunchCodex.ReversedMap.TryGetValue(options.DataStorage, out Dictionary<OpVal, char>? codex))
            throw new FormatException($"Unsupported DataStorage: {options.DataStorage}");

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