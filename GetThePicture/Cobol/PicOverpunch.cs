using System.Collections.ObjectModel;

namespace GetThePicture.Cobol;

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

public readonly struct DigitInfo(int sign, char digit)
{
    public int Sign { get; } = sign;
    public char Digit { get; } = digit;
}

/// <summary>
/// Data Description Entry : <see href="https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html">USAGE Clause</see>
/// </summary>
public static class OverpunchTables
{
    /// <summary>
    /// -Dca, -Dcb, -Dcm, -Dcr
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_POSITIVE_01  = new()
    {
        { '0', new DigitInfo(1, '0') },
        { '1', new DigitInfo(1, '1') },
        { '2', new DigitInfo(1, '2') },
        { '3', new DigitInfo(1, '3') },
        { '4', new DigitInfo(1, '4') },
        { '5', new DigitInfo(1, '5') },
        { '6', new DigitInfo(1, '6') },
        { '7', new DigitInfo(1, '7') },
        { '8', new DigitInfo(1, '8') },
        { '9', new DigitInfo(1, '9') },
    };

    /// <summary>
    /// -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_POSITIVE_02  = new()
    {
        { '{', new DigitInfo(1, '0') },
        { 'A', new DigitInfo(1, '1') },
        { 'B', new DigitInfo(1, '2') },
        { 'C', new DigitInfo(1, '3') },
        { 'D', new DigitInfo(1, '4') },
        { 'E', new DigitInfo(1, '5') },
        { 'F', new DigitInfo(1, '6') },
        { 'G', new DigitInfo(1, '7') },
        { 'H', new DigitInfo(1, '8') },
        { 'I', new DigitInfo(1, '9') },
    };

    /// <summary>
    /// -Dca, -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_NEGATIVE_01 = new()
    {
        { '}', new DigitInfo(-1, '0') },
        { 'J', new DigitInfo(-1, '1') },
        { 'K', new DigitInfo(-1, '2') },
        { 'L', new DigitInfo(-1, '3') },
        { 'M', new DigitInfo(-1, '4') },
        { 'N', new DigitInfo(-1, '5') },
        { 'O', new DigitInfo(-1, '6') },
        { 'P', new DigitInfo(-1, '7') },
        { 'Q', new DigitInfo(-1, '8') },
        { 'R', new DigitInfo(-1, '9') },
    };

    /// <summary>
    /// -Dcb
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_NEGATIVE_02 = new()
    {
        { '@', new DigitInfo(-1, '0') },
        { 'A', new DigitInfo(-1, '1') },
        { 'B', new DigitInfo(-1, '2') },
        { 'C', new DigitInfo(-1, '3') },
        { 'D', new DigitInfo(-1, '4') },
        { 'E', new DigitInfo(-1, '5') },
        { 'F', new DigitInfo(-1, '6') },
        { 'G', new DigitInfo(-1, '7') },
        { 'H', new DigitInfo(-1, '8') },
        { 'I', new DigitInfo(-1, '9') },
    };

    /// <summary>
    /// -Dcm
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_NEGATIVE_03 = new()
    {
        { 'p', new DigitInfo(-1, '0') },
        { 'q', new DigitInfo(-1, '1') },
        { 'r', new DigitInfo(-1, '2') },
        { 's', new DigitInfo(-1, '3') },
        { 't', new DigitInfo(-1, '4') },
        { 'u', new DigitInfo(-1, '5') },
        { 'v', new DigitInfo(-1, '6') },
        { 'w', new DigitInfo(-1, '7') },
        { 'x', new DigitInfo(-1, '8') },
        { 'y', new DigitInfo(-1, '9') },
    };

    /// <summary>
    /// -Dcr
    /// </summary>
    public static readonly Dictionary<char, DigitInfo> OP_NEGATIVE_04 = new()
    {
        { ' ', new DigitInfo(-1, '0') }, // (space)
        { '!', new DigitInfo(-1, '1') },
        { '"', new DigitInfo(-1, '2') }, // (double-quote)
        { '#', new DigitInfo(-1, '3') },
        { '$', new DigitInfo(-1, '4') },
        { '%', new DigitInfo(-1, '5') },
        { '&', new DigitInfo(-1, '6') },
        { '\'',new DigitInfo(-1, '7') }, // (single-quote)
        { '(', new DigitInfo(-1, '8') },
        { ')', new DigitInfo(-1, '9') },
    };
}

/// <summary>
/// 
/// </summary>
public static class OverpunchCode
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<char, DigitInfo>> Map;

    static OverpunchCode()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<char, DigitInfo>>
        {
            { DataStorageOptions.CA, Merge(OverpunchTables.OP_POSITIVE_01, OverpunchTables.OP_NEGATIVE_01) },
            { DataStorageOptions.CB, Merge(OverpunchTables.OP_POSITIVE_01, OverpunchTables.OP_NEGATIVE_02) },
            { DataStorageOptions.CI, Merge(OverpunchTables.OP_POSITIVE_02, OverpunchTables.OP_NEGATIVE_01) },
            { DataStorageOptions.CM, Merge(OverpunchTables.OP_POSITIVE_01, OverpunchTables.OP_NEGATIVE_03) },
            { DataStorageOptions.CN, Merge(OverpunchTables.OP_POSITIVE_02, OverpunchTables.OP_NEGATIVE_01) },
            { DataStorageOptions.CR, Merge(OverpunchTables.OP_POSITIVE_01, OverpunchTables.OP_NEGATIVE_04) },
        };
        
        Map = new ReadOnlyDictionary<DataStorageOptions, Dictionary<char, DigitInfo>>(dictionary);
    }

    private static Dictionary<char, DigitInfo> Merge(
        Dictionary<char, DigitInfo> a,
        Dictionary<char, DigitInfo> b)
    {
        var dict = new Dictionary<char, DigitInfo>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }
}
