using System.Collections.ObjectModel;

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
/// Overpunch Value
/// </summary>
/// <param name="sign"></param>
/// <param name="digit"></param>
public readonly struct OpVal(decimal sign, char digit)
{
    public decimal Sign { get; } = sign;
    public char Digit { get; } = digit;
}

/// <summary>
/// Overpunch Codex<br/>
/// Data Description Entry : <see href="https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html">USAGE Clause</see>
/// </summary>
public static class OpCodex
{
    /// <summary>
    /// -Dca, -Dcb, -Dcm, -Dcr
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_POSITIVE_01  = new()
    {
        { '0', new OpVal(1.0m, '0') },
        { '1', new OpVal(1.0m, '1') },
        { '2', new OpVal(1.0m, '2') },
        { '3', new OpVal(1.0m, '3') },
        { '4', new OpVal(1.0m, '4') },
        { '5', new OpVal(1.0m, '5') },
        { '6', new OpVal(1.0m, '6') },
        { '7', new OpVal(1.0m, '7') },
        { '8', new OpVal(1.0m, '8') },
        { '9', new OpVal(1.0m, '9') },
    };

    /// <summary>
    /// -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_POSITIVE_02  = new()
    {
        { '{', new OpVal(1.0m, '0') },
        { 'A', new OpVal(1.0m, '1') },
        { 'B', new OpVal(1.0m, '2') },
        { 'C', new OpVal(1.0m, '3') },
        { 'D', new OpVal(1.0m, '4') },
        { 'E', new OpVal(1.0m, '5') },
        { 'F', new OpVal(1.0m, '6') },
        { 'G', new OpVal(1.0m, '7') },
        { 'H', new OpVal(1.0m, '8') },
        { 'I', new OpVal(1.0m, '9') },
    };

    /// <summary>
    /// -Dca, -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_NEGATIVE_01 = new()
    {
        { '}', new OpVal(-1.0m, '0') },
        { 'J', new OpVal(-1.0m, '1') },
        { 'K', new OpVal(-1.0m, '2') },
        { 'L', new OpVal(-1.0m, '3') },
        { 'M', new OpVal(-1.0m, '4') },
        { 'N', new OpVal(-1.0m, '5') },
        { 'O', new OpVal(-1.0m, '6') },
        { 'P', new OpVal(-1.0m, '7') },
        { 'Q', new OpVal(-1.0m, '8') },
        { 'R', new OpVal(-1.0m, '9') },
    };

    /// <summary>
    /// -Dcb
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_NEGATIVE_02 = new()
    {
        { '@', new OpVal(-1.0m, '0') },
        { 'A', new OpVal(-1.0m, '1') },
        { 'B', new OpVal(-1.0m, '2') },
        { 'C', new OpVal(-1.0m, '3') },
        { 'D', new OpVal(-1.0m, '4') },
        { 'E', new OpVal(-1.0m, '5') },
        { 'F', new OpVal(-1.0m, '6') },
        { 'G', new OpVal(-1.0m, '7') },
        { 'H', new OpVal(-1.0m, '8') },
        { 'I', new OpVal(-1.0m, '9') },
    };

    /// <summary>
    /// -Dcm
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_NEGATIVE_03 = new()
    {
        { 'p', new OpVal(-1.0m, '0') },
        { 'q', new OpVal(-1.0m, '1') },
        { 'r', new OpVal(-1.0m, '2') },
        { 's', new OpVal(-1.0m, '3') },
        { 't', new OpVal(-1.0m, '4') },
        { 'u', new OpVal(-1.0m, '5') },
        { 'v', new OpVal(-1.0m, '6') },
        { 'w', new OpVal(-1.0m, '7') },
        { 'x', new OpVal(-1.0m, '8') },
        { 'y', new OpVal(-1.0m, '9') },
    };

    /// <summary>
    /// -Dcr
    /// </summary>
    public static readonly Dictionary<char, OpVal> OP_NEGATIVE_04 = new()
    {
        { ' ', new OpVal(-1.0m, '0') }, // (space)
        { '!', new OpVal(-1.0m, '1') },
        { '"', new OpVal(-1.0m, '2') }, // (double-quote)
        { '#', new OpVal(-1.0m, '3') },
        { '$', new OpVal(-1.0m, '4') },
        { '%', new OpVal(-1.0m, '5') },
        { '&', new OpVal(-1.0m, '6') },
        { '\'',new OpVal(-1.0m, '7') }, // (single-quote)
        { '(', new OpVal(-1.0m, '8') },
        { ')', new OpVal(-1.0m, '9') },
    };
}

/// <summary>
/// 
/// </summary>
public static class OverpunchCode
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<char, OpVal>> Map;

    static OverpunchCode()
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
        
        Map = new ReadOnlyDictionary<DataStorageOptions, Dictionary<char, OpVal>>(dictionary);
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
}
