using GetThePicture.Copybook.Wrapper.Base;
using GetThePicture.Picture.Clause;

namespace GetThePicture.Copybook.Wrapper;

/// <summary>
/// 抽象 Copybook wrapper base class
/// 提供 Indexer、Read/Write、零複製操作
/// </summary>
public abstract class CbWrapper : IWrapper
{
    // ----------------------------
    // Interface
    // ----------------------------

    /// <summary>底層 buffer</summary>
    public byte[] Raw => _raw;

    /// <summary>
    /// 透過欄位名稱存取資料
    /// <code>
    /// var wrapper = CbWrapper(100);
    /// 
    /// wrapper["FIELD1"] = 1234;
    /// wrapper["FIELD2"] = "Hello!";
    /// 
    /// int field1 = (int)wrapper["FIELD1"];
    /// string field2 = (string)wrapper["FIELD2"];
    /// </code>
    /// </summary>
    public object this[string name]
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
    // Wrapper
    // ----------------------------

    public CbWrapper(byte[]? raw = null)
    {
        _raw = raw is null ? new byte[RequiredBufferLength] : (byte[])raw.Clone();

        ValidateLayout();
    }

    // (真身)
    private readonly byte[] _raw;

    /// <summary>
    /// 子類別必須提供欄位映射
    /// </summary>
    protected abstract Dictionary<string, CbAddress> AddressMap { get; }

    /// <summary>
    /// 計算 Copybook 佈局所需的最小 Raw buffer 長度。 <br/>
    /// 以最大欄位的 (Start + Length) 為基準，
    /// Start 為 1-based，因此需扣除 1。
    /// </summary>
    protected int RequiredBufferLength => AddressMap.Values.Max(a => a.Start + a.Length) - 1; // 1-based

    /// <summary>
    /// 驗證目前 Raw buffer 是否足以容納整個 Copybook 佈局 (AddressMap)。 <br/>
    /// 若長度不足，代表資料不完整或 Copybook 定義錯誤。
    /// </summary>
    protected void ValidateLayout()
    {
        int required = RequiredBufferLength;

        if (_raw.Length < required)
        {
            throw new InvalidOperationException($"Raw length {_raw.Length} is smaller than required {required}.");
        }
    }

    /// <summary>讀取欄位值 (零複製)</summary>
    protected object Read(CbAddress addr)
    {
        var buffer = _raw.AsSpan(addr.Start, addr.Length);

        object value = PicClauseCodec.ForMeta(addr.Meta).WithStrict().Decode(buffer);
        return value;
    }

    /// <summary>寫入欄位值 (零複製)</summary>
    protected int Write(object value, CbAddress addr)
    {
        Span<byte> bytes = PicClauseCodec.ForMeta(addr.Meta).WithStrict().Encode(value);

        if (bytes.Length != addr.Length)
            throw new ArgumentException($"Encoded length {bytes.Length} does not match expected length {addr.Length}");

        bytes.CopyTo(_raw.AsSpan(addr.Start, addr.Length));
        return bytes.Length;
    }
}
