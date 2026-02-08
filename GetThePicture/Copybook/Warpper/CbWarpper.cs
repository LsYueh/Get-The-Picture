using GetThePicture.Copybook.Warpper.Base;
using GetThePicture.Picture.Clause;

namespace GetThePicture.Copybook.Warpper;

/// <summary>
/// 抽象 Copybook wrapper base class
/// 提供 Indexer、Read/Write、零複製操作
/// </summary>
public abstract class CbWarpper : IWarpper
{
    // ----------------------------
    // Interface
    // ----------------------------

    /// <summary>底層 buffer</summary>
    public byte[] Raw => _raw;

    /// <summary>
    /// 透過欄位名稱存取資料
    /// <code>
    /// var wrapper = CbWarpper(100);
    /// 
    /// wrapper["FIELD1"] = 1234;
    /// wrapper["FIELD2"] = "Hello!";
    /// 
    /// int field1 = (int)wrapper["FIELD1"];
    /// string field2 = (string)wrapper["FIELD2"];
    /// </code>
    /// </summary>
    public object? this[string name]
    {
        get
        {
            if (!AddressMap.TryGetValue(name, out var addr))
                throw new KeyNotFoundException($"Field '{name}' does not exist.");

            return Read(addr);
        }
        set
        {
            ArgumentNullException.ThrowIfNull(value);

            if (!AddressMap.TryGetValue(name, out var addr))
                throw new KeyNotFoundException($"Field '{name}' does not exist.");

            _ = Write(value, addr);
        }
    }

    // ----------------------------
    // WarpperBase
    // ----------------------------

    /// <summary>用既有 buffer 建構</summary>
    public CbWarpper(byte[] raw)
    {
        _raw = (byte[])raw.Clone();
    }

    /// <summary>建構新 buffer</summary>
    public CbWarpper(int length)
    {
        _raw = new byte[length];
    }

    private readonly byte[] _raw;

    /// <summary>
    /// 子類別必須提供欄位映射
    /// </summary>
    protected abstract Dictionary<string, CbAddress> AddressMap { get; }

    /// <summary>讀取欄位值 (零複製)</summary>
    object? Read(CbAddress addr)
    {
        var buffer = _raw.AsSpan(addr.Start, addr.Length);

        object? value = PicClauseCodec.ForMeta(addr.Meta).Decode(buffer);
        return value;
    }

    /// <summary>寫入欄位值 (零複製)</summary>
    int Write(object value, CbAddress addr)
    {
        Span<byte> bytes = PicClauseCodec.ForMeta(addr.Meta).WithStrict().Encode(value);

        if (bytes.Length != addr.Length)
            throw new ArgumentException($"Encoded length {bytes.Length} does not match expected length {addr.Length}");

        bytes.CopyTo(_raw.AsSpan(addr.Start, addr.Length));
        return bytes.Length;
    }
}
