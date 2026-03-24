using GetThePicture.Copybook.Wrapper.Base;

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
