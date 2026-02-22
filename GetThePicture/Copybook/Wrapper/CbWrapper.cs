using GetThePicture.Copybook.Wrapper.Base;
using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Copybook.Wrapper;

/// <summary>
/// 表示 <see cref="CbWrapper"/> 中的單一欄位存取代理。
/// <para>
/// <c>CbField</c> 封裝指定欄位的位址資訊與讀寫行為，
/// 提供型別安全的 <c>Get&lt;T&gt;()</c>、<c>Set&lt;T&gt;()</c>
/// 以及 <c>Clear()</c> 操作。
/// </para>
/// <para>
/// 此型別為 <see langword="readonly struct"/>，
/// 設計為輕量級值型別，以避免在頻繁欄位操作時產生額外的堆積配置。
/// </para>
/// <para>
/// 注意：欄位不支援 <c>null</c> 語意，
/// 清除欄位時將依 PIC 類型寫入對應的預設值（例如 SPACES 或 ZEROS）。
/// </para>
/// </summary>
public readonly struct CbField
{
    private readonly CbWrapper _wrapper;
    private readonly CbAddress _addr;

    internal CbField(CbWrapper wrapper, CbAddress addr)
    {
        _wrapper = wrapper;
        _addr = addr;
    }

    public T Get<T>() => (T)_wrapper.Read(_addr);

    public void Set<T>(T value) => _wrapper.Write(value!, _addr);

    public void Clear() => _wrapper.WriteDefault(_addr);
}

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
    /// <example>
    /// <code>
    /// var wrapper = CbWrapper(100);
    /// 
    /// wrapper["FIELD1"].Set(1234);
    /// wrapper["FIELD2"].Set("Hello!");
    /// 
    /// int field1 = wrapper["FIELD1"].Get&lt;int&gt;();
    /// string field2 = wrapper["FIELD2"].Get&lt;string&gt;();
    /// </code>
    /// </example>
    /// </summary>
    /// <param name="name">欄位名稱。</param>
    /// <returns>對應的 <see cref="CbField"/>。</returns>
    /// <exception cref="KeyNotFoundException">
    /// 當指定欄位不存在時拋出。
    /// </exception>
    public CbField this[string name]
    {
        get
        {
            if (!AddressMap.TryGetValue(name, out var addr))
                throw new KeyNotFoundException($"Field '{name}' does not exist.");

            return new CbField(this, addr);
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
    private int RequiredBufferLength => AddressMap.Values.Max(a => a.Start + a.Length);

    /// <summary>
    /// 依欄位名稱取得對應的 <see cref="CbField"/> 存取代理。
    /// <para>
    /// 此方法提供更具語意的欄位存取方式，
    /// 適用於需要明確表達「操作欄位」語意的情境。
    /// </para>
    /// <para>
    /// 與 <c>this[string]</c> 等價：
    /// </para>
    /// <code>
    /// wrapper.Field("NAME").Set("HELLO");
    /// var value = wrapper.Field("NAME").Get&lt;string&gt;();
    /// </code>
    /// </summary>
    /// <param name="name">欄位名稱。</param>
    /// <returns>對應的 <see cref="CbField"/>。</returns>
    /// <exception cref="KeyNotFoundException">
    /// 當指定欄位不存在時拋出。
    /// </exception>
    public CbField Field(string name) => this[name];

    private CbField[]? _fields;
    public CbField[] Fields =>  _fields ??= [.. AddressMap.Values.Select(addr => new CbField(this, addr))];

    /// <summary>
    /// 驗證目前 Raw buffer 是否足以容納整個 Copybook 佈局 (AddressMap)。 <br/>
    /// 若長度不足，代表資料不完整或 Copybook 定義錯誤。
    /// </summary>
    private void ValidateLayout()
    {
        int required = RequiredBufferLength;

        if (_raw.Length < required)
        {
            throw new InvalidOperationException($"Raw length {_raw.Length} is smaller than required {required}.");
        }
    }

    /// <summary>
    /// 讀取欄位值
    /// </summary>
    /// <param name="addr"></param>
    /// <returns></returns>
    internal object Read(CbAddress addr)
    {
        var buffer = _raw.AsSpan(addr.Start, addr.Length);

        object value = PicClauseCodec.ForMeta(addr.Meta).WithStrict().Decode(buffer);
        return value;
    }

    /// <summary>
    /// 寫入欄位值
    /// </summary>
    /// <param name="value"></param>
    /// <param name="addr"></param>
    /// <returns></returns>
    /// <exception cref="ArgumentException"></exception>
    internal int Write(object value, CbAddress addr)
    {
        Span<byte> bytes = PicClauseCodec.ForMeta(addr.Meta).WithStrict().Encode(value);

        if (bytes.Length != addr.Length)
            throw new ArgumentException($"Encoded length {bytes.Length} does not match expected length {addr.Length}");

        bytes.CopyTo(_raw.AsSpan(addr.Start, addr.Length));
        return bytes.Length;
    }

    /// <summary>
    /// 將指定欄位重設為其 COBOL 預設值。
    /// <para>
    /// 預設值將依 PIC 類型決定：
    /// </para>
    /// <list type="bullet">
    /// <item>
    /// <description>Alphabetic / Alphanumeric → SPACES</description>
    /// </item>
    /// <item>
    /// <description>Numeric → ZEROS</description>
    /// </item>
    /// </list>
    /// </summary>
    /// <param name="addr"></param>
    /// <exception cref="NotImplementedException"></exception>
    /// <exception cref="NotSupportedException"></exception>
    internal int WriteDefault(CbAddress addr)
    {
        Span<byte> bytes = PicClauseCodec.ForMeta(addr.Meta).WithStrict().CreateDefaultRepresentation();

        if (bytes.Length != addr.Length)
            throw new InvalidOperationException($"Initialized length {bytes.Length} does not match expected length {addr.Length}");

        bytes.CopyTo(_raw.AsSpan(addr.Start, addr.Length));
        return bytes.Length;
    }
}
