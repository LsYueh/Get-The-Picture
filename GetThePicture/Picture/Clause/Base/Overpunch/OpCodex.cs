using System.Collections.ObjectModel;

using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Base.Overpunch;

/// <summary>
/// Overpunch Codex
/// </summary>
internal static class OpCodex
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, OpVal>> Map;
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, byte>> ReversedMap;

    static OpCodex()
    {
        Map = BuildMap();
        ReversedMap = BuildReversedMap();
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, OpVal>> BuildMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<byte, OpVal>>
        {
            { DataStorageOptions.CA, Merge(OpCodexBase.OP_POSITIVE_01, OpCodexBase.OP_NEGATIVE_01) },
            { DataStorageOptions.CB, Merge(OpCodexBase.OP_POSITIVE_01, OpCodexBase.OP_NEGATIVE_02) },
            { DataStorageOptions.CI, Merge(OpCodexBase.OP_POSITIVE_02, OpCodexBase.OP_NEGATIVE_01) },
            { DataStorageOptions.CM, Merge(OpCodexBase.OP_POSITIVE_01, OpCodexBase.OP_NEGATIVE_03) },
            { DataStorageOptions.CN, Merge(OpCodexBase.OP_POSITIVE_02, OpCodexBase.OP_NEGATIVE_01) },
            { DataStorageOptions.CR, Merge(OpCodexBase.OP_POSITIVE_01, OpCodexBase.OP_NEGATIVE_04) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, OpVal>>(dictionary);
    }

    private static Dictionary<byte, OpVal> Merge(
        Dictionary<byte, OpVal> a,
        Dictionary<byte, OpVal> b)
    {
        var dict = new Dictionary<byte, OpVal>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, byte>> BuildReversedMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<OpVal, byte>>
        {
            { DataStorageOptions.CA, MergeRev(OpCodexBase.OP_POSITIVE_01_REVERSE, OpCodexBase.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CB, MergeRev(OpCodexBase.OP_POSITIVE_01_REVERSE, OpCodexBase.OP_NEGATIVE_02_REVERSE) },
            { DataStorageOptions.CI, MergeRev(OpCodexBase.OP_POSITIVE_02_REVERSE, OpCodexBase.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CM, MergeRev(OpCodexBase.OP_POSITIVE_01_REVERSE, OpCodexBase.OP_NEGATIVE_03_REVERSE) },
            { DataStorageOptions.CN, MergeRev(OpCodexBase.OP_POSITIVE_02_REVERSE, OpCodexBase.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CR, MergeRev(OpCodexBase.OP_POSITIVE_01_REVERSE, OpCodexBase.OP_NEGATIVE_04_REVERSE) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<OpVal, byte>>(dictionary);
    }

    private static Dictionary<OpVal, byte> MergeRev(
        Dictionary<OpVal, byte> a,
        Dictionary<OpVal, byte> b)
    {
        var dict = new Dictionary<OpVal, byte>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }
}

/// <summary>
/// Overpunch Value
/// </summary>
/// <param name="sign"></param>
/// <param name="digit"></param>
internal readonly struct OpVal(decimal sign, byte digit)
{
    public decimal Sign { get; } = sign;
    public byte Digit { get; } = digit;
}

/// <summary>
/// Overpunch Codex<br/>
/// Data Description Entry : <see href="https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html">USAGE Clause</see>
/// </summary>
internal static class OpCodexBase
{
    /// <summary>
    /// -Dca, -Dcb, -Dcm, -Dcr
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_POSITIVE_01 = new()
    {
        { (byte)'0', new OpVal(1.0m, (byte)'0') },
        { (byte)'1', new OpVal(1.0m, (byte)'1') },
        { (byte)'2', new OpVal(1.0m, (byte)'2') },
        { (byte)'3', new OpVal(1.0m, (byte)'3') },
        { (byte)'4', new OpVal(1.0m, (byte)'4') },
        { (byte)'5', new OpVal(1.0m, (byte)'5') },
        { (byte)'6', new OpVal(1.0m, (byte)'6') },
        { (byte)'7', new OpVal(1.0m, (byte)'7') },
        { (byte)'8', new OpVal(1.0m, (byte)'8') },
        { (byte)'9', new OpVal(1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_POSITIVE_01_REVERSE = OP_POSITIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_POSITIVE_02 = new()
    {
        { (byte)'{', new OpVal(1.0m, (byte)'0') },
        { (byte)'A', new OpVal(1.0m, (byte)'1') },
        { (byte)'B', new OpVal(1.0m, (byte)'2') },
        { (byte)'C', new OpVal(1.0m, (byte)'3') },
        { (byte)'D', new OpVal(1.0m, (byte)'4') },
        { (byte)'E', new OpVal(1.0m, (byte)'5') },
        { (byte)'F', new OpVal(1.0m, (byte)'6') },
        { (byte)'G', new OpVal(1.0m, (byte)'7') },
        { (byte)'H', new OpVal(1.0m, (byte)'8') },
        { (byte)'I', new OpVal(1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_POSITIVE_02_REVERSE = OP_POSITIVE_02.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dca, -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_NEGATIVE_01 = new()
    {
        { (byte)'}', new OpVal(-1.0m, (byte)'0') },
        { (byte)'J', new OpVal(-1.0m, (byte)'1') },
        { (byte)'K', new OpVal(-1.0m, (byte)'2') },
        { (byte)'L', new OpVal(-1.0m, (byte)'3') },
        { (byte)'M', new OpVal(-1.0m, (byte)'4') },
        { (byte)'N', new OpVal(-1.0m, (byte)'5') },
        { (byte)'O', new OpVal(-1.0m, (byte)'6') },
        { (byte)'P', new OpVal(-1.0m, (byte)'7') },
        { (byte)'Q', new OpVal(-1.0m, (byte)'8') },
        { (byte)'R', new OpVal(-1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_NEGATIVE_01_REVERSE = OP_NEGATIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcb
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_NEGATIVE_02 = new()
    {
        { (byte)'@', new OpVal(-1.0m, (byte)'0') },
        { (byte)'A', new OpVal(-1.0m, (byte)'1') },
        { (byte)'B', new OpVal(-1.0m, (byte)'2') },
        { (byte)'C', new OpVal(-1.0m, (byte)'3') },
        { (byte)'D', new OpVal(-1.0m, (byte)'4') },
        { (byte)'E', new OpVal(-1.0m, (byte)'5') },
        { (byte)'F', new OpVal(-1.0m, (byte)'6') },
        { (byte)'G', new OpVal(-1.0m, (byte)'7') },
        { (byte)'H', new OpVal(-1.0m, (byte)'8') },
        { (byte)'I', new OpVal(-1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_NEGATIVE_02_REVERSE = OP_NEGATIVE_02.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcm
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_NEGATIVE_03 = new()
    {
        { (byte)'p', new OpVal(-1.0m, (byte)'0') },
        { (byte)'q', new OpVal(-1.0m, (byte)'1') },
        { (byte)'r', new OpVal(-1.0m, (byte)'2') },
        { (byte)'s', new OpVal(-1.0m, (byte)'3') },
        { (byte)'t', new OpVal(-1.0m, (byte)'4') },
        { (byte)'u', new OpVal(-1.0m, (byte)'5') },
        { (byte)'v', new OpVal(-1.0m, (byte)'6') },
        { (byte)'w', new OpVal(-1.0m, (byte)'7') },
        { (byte)'x', new OpVal(-1.0m, (byte)'8') },
        { (byte)'y', new OpVal(-1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_NEGATIVE_03_REVERSE = OP_NEGATIVE_03.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcr
    /// </summary>
    public static readonly Dictionary<byte, OpVal> OP_NEGATIVE_04 = new()
    {
        { (byte)' ', new OpVal(-1.0m, (byte)'0') }, // (space)
        { (byte)'!', new OpVal(-1.0m, (byte)'1') },
        { (byte)'"', new OpVal(-1.0m, (byte)'2') }, // (double-quote)
        { (byte)'#', new OpVal(-1.0m, (byte)'3') },
        { (byte)'$', new OpVal(-1.0m, (byte)'4') },
        { (byte)'%', new OpVal(-1.0m, (byte)'5') },
        { (byte)'&', new OpVal(-1.0m, (byte)'6') },
        { (byte)'\'',new OpVal(-1.0m, (byte)'7') }, // (single-quote)
        { (byte)'(', new OpVal(-1.0m, (byte)'8') },
        { (byte)')', new OpVal(-1.0m, (byte)'9') },
    };

    public static readonly Dictionary<OpVal, byte> OP_NEGATIVE_04_REVERSE = OP_NEGATIVE_04.ToDictionary(kv => kv.Value, kv => kv.Key);
}
