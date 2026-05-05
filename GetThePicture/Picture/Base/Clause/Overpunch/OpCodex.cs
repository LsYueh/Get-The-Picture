using System.Collections.ObjectModel;

using GetThePicture.Picture.Base.Clause.Options;

namespace GetThePicture.Picture.Base.Clause.Overpunch;

/// <summary>
/// Overpunch Codex
/// </summary>
public static class OpCodex
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, string>> Map;
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<string, byte>> ReversedMap;

    static OpCodex()
    {
        Map = BuildMap();
        ReversedMap = BuildReversedMap();
    }

    /// <summary>
    /// Get Overpunch Value
    /// </summary>
    /// <param name="key"></param>
    /// <param name="ds">DataStorage Options</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static string TryGetValue(byte key, DataStorageOptions ds)
    {
        if (!Map.TryGetValue(ds, out Dictionary<byte, string>? codex))
            throw new FormatException($"Unsupported DataStorage: {ds}");

        if (!codex.TryGetValue(key, out string? opValue))
            throw new FormatException($"Invalid overpunch search key: '{key}'");

        return opValue;
    }

    /// <summary>
    /// Get Overpunch Key
    /// </summary>
    /// <param name="opValue"></param>
    /// <param name="ds">DataStorage Options</param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static byte TryGetKey(string opValue, DataStorageOptions ds)
    {
        if (!ReversedMap.TryGetValue(ds, out Dictionary<string, byte>? codex))
            throw new FormatException($"Unsupported DataStorage: {ds}");

        if (!codex.TryGetValue(opValue, out byte key))
            throw new FormatException($"Invalid overpunch search value: '{opValue}'");

        return key;
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, string>> BuildMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<byte, string>>
        {
            { DataStorageOptions.CA, Merge(OpTable.OP_POSITIVE_01, OpTable.OP_NEGATIVE_01) },
            { DataStorageOptions.CB, Merge(OpTable.OP_POSITIVE_01, OpTable.OP_NEGATIVE_02) },
            { DataStorageOptions.CI, Merge(OpTable.OP_POSITIVE_02, OpTable.OP_NEGATIVE_01) },
            { DataStorageOptions.CM, Merge(OpTable.OP_POSITIVE_01, OpTable.OP_NEGATIVE_03) },
            { DataStorageOptions.CN, Merge(OpTable.OP_POSITIVE_02, OpTable.OP_NEGATIVE_01) },
            { DataStorageOptions.CR, Merge(OpTable.OP_POSITIVE_01, OpTable.OP_NEGATIVE_04) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, string>>(dictionary);
    }

    private static Dictionary<byte, string> Merge(
        Dictionary<byte, string> a,
        Dictionary<byte, string> b)
    {
        var dict = new Dictionary<byte, string>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<string, byte>> BuildReversedMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<string, byte>>
        {
            { DataStorageOptions.CA, MergeRev(OpTable.OP_POSITIVE_01_REVERSE, OpTable.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CB, MergeRev(OpTable.OP_POSITIVE_01_REVERSE, OpTable.OP_NEGATIVE_02_REVERSE) },
            { DataStorageOptions.CI, MergeRev(OpTable.OP_POSITIVE_02_REVERSE, OpTable.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CM, MergeRev(OpTable.OP_POSITIVE_01_REVERSE, OpTable.OP_NEGATIVE_03_REVERSE) },
            { DataStorageOptions.CN, MergeRev(OpTable.OP_POSITIVE_02_REVERSE, OpTable.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CR, MergeRev(OpTable.OP_POSITIVE_01_REVERSE, OpTable.OP_NEGATIVE_04_REVERSE) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<string, byte>>(dictionary);
    }

    private static Dictionary<string, byte> MergeRev(
        Dictionary<string, byte> a,
        Dictionary<string, byte> b)
    {
        var dict = new Dictionary<string, byte>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        return dict;
    }
}
