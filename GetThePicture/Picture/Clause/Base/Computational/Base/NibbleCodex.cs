using System.Collections.ObjectModel;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Base.Computational.Base;

/// <summary>
/// Ref. <see href="https://docs.rocketsoftware.com/bundle/acucobolgt_dg_1050_html/page/BKUSUSCOMPUS2.2.10.html">Data Storage Options</see>
/// </summary>
internal static class NibbleCodex
{
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<NibbleSign, byte>> Map;
    public static readonly ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, NibbleSign>> ReversedMap;

    static NibbleCodex()
    {
        Map = BuildMap();
        ReversedMap = BuildReversedMap();
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<NibbleSign, byte>> BuildMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<NibbleSign, byte>>
        {
            { DataStorageOptions.CA, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_01, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CB, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_02, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CI, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_02, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CM, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_02, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CN, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_03, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CR, Merge(NiCodex.OP_UNSIGNED_01, NiCodex.OP_POSITIVE_02, NiCodex.OP_NEGATIVE_01) },
            { DataStorageOptions.CV, Merge(NiCodex.OP_UNSIGNED_02, NiCodex.OP_POSITIVE_02, NiCodex.OP_NEGATIVE_01) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<NibbleSign, byte>>(dictionary);
    }

    private static Dictionary<NibbleSign, byte> Merge(Dictionary<NibbleSign, byte> a, Dictionary<NibbleSign, byte> b, Dictionary<NibbleSign, byte> c)
    {
        var dict = new Dictionary<NibbleSign, byte>(a.Count + b.Count + c.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        foreach (var kv in c) dict[kv.Key] = kv.Value;
        return dict;
    }

    private static ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, NibbleSign>> BuildReversedMap()
    {
        var dictionary = new Dictionary<DataStorageOptions, Dictionary<byte, NibbleSign>>
        {
            { DataStorageOptions.CA, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_01_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CB, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_02_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CI, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_02_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CM, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_02_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CN, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_03_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CR, MergeRev(NiCodex.OP_UNSIGNED_01_REVERSE, NiCodex.OP_POSITIVE_02_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
            { DataStorageOptions.CV, MergeRev(NiCodex.OP_UNSIGNED_02_REVERSE, NiCodex.OP_POSITIVE_02_REVERSE, NiCodex.OP_NEGATIVE_01_REVERSE) },
        };
        
        return new ReadOnlyDictionary<DataStorageOptions, Dictionary<byte, NibbleSign>>(dictionary);
    }

    private static Dictionary<byte, NibbleSign> MergeRev(Dictionary<byte, NibbleSign> a, Dictionary<byte, NibbleSign> b, Dictionary<byte, NibbleSign> c)
    {
        var dict = new Dictionary<byte, NibbleSign>(a.Count + b.Count);
        foreach (var kv in a) dict[kv.Key] = kv.Value;
        foreach (var kv in b) dict[kv.Key] = kv.Value;
        foreach (var kv in c) dict[kv.Key] = kv.Value;
        return dict;
    }
}

/// <summary>
/// `SIGN IS TRAILING` is the default.
/// </summary>
public enum NibbleSign {
    Positive,
    Negative,
    Unsigned,
}

/// <summary>
/// Sign - Nibble
/// </summary>
public static class NiCodex
{
    /// <summary>
    /// -Dca (0x0F)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_POSITIVE_01 = new()
    {
        { NibbleSign.Positive, 0x1F }, // Note: 要跟UNSIGNED的0x0F在反向Dictionary做區別
    };

    public static readonly Dictionary<byte, NibbleSign> OP_POSITIVE_01_REVERSE = OP_POSITIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcb/-Dci/-Dcm/-Dcr (0x0C)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_POSITIVE_02 = new()
    {
        { NibbleSign.Positive, 0x0C },
    };

    public static readonly Dictionary<byte, NibbleSign> OP_POSITIVE_02_REVERSE = OP_POSITIVE_02.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcn (0x0B)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_POSITIVE_03 = new()
    {
        { NibbleSign.Positive, 0x0B },
    };

    public static readonly Dictionary<byte, NibbleSign> OP_POSITIVE_03_REVERSE = OP_POSITIVE_03.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dca/-Dcb/-Dci/-Dcm/-Dcr (0x0D)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_NEGATIVE_01 = new()
    {
        { NibbleSign.Negative, 0x0D },
    };

    public static readonly Dictionary<byte, NibbleSign> OP_NEGATIVE_01_REVERSE = OP_NEGATIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dca/-Dcb/-Dci/-Dcm/-Dcr (0x0F)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_UNSIGNED_01 = new()
    {
        { NibbleSign.Unsigned, 0x0F },
    };

    public static readonly Dictionary<byte, NibbleSign> OP_UNSIGNED_01_REVERSE = OP_UNSIGNED_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcv (0x0C)
    /// </summary>
    public static readonly Dictionary<NibbleSign, byte> OP_UNSIGNED_02 = new()
    {
        { NibbleSign.Unsigned, 0x1C }, // Note: 要跟POSITIVE的0x0C在反向Dictionary做區別
    };

    public static readonly Dictionary<byte, NibbleSign> OP_UNSIGNED_02_REVERSE = OP_UNSIGNED_02.ToDictionary(kv => kv.Value, kv => kv.Key);
}
