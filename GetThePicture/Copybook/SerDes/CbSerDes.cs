using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.Copybook.SerDes.Provider;

namespace GetThePicture.Copybook.SerDes;

/// <summary>
/// 已棄用：SerDes 功能已被 Warpper 取代，請改使用 CbWarpper。
/// </summary>
[Obsolete("已棄用，請使用 CbWarpper 讀寫 Raw Buffer", false)]
public sealed class CbSerDes
{
    private readonly CbStorage _storage;

    private CbDeserializer _deserializer;
    private CbSerializer _serializer;

    public CbSerDes(IDataProvider provider)
    {
        _storage = provider.GetStorage();

        _deserializer = new(_storage);
        _serializer   = new(_storage);
    }

    /// <summary>
    /// Deserialize a single record according to Copybook layout.
    /// </summary>
    /// <param name="record"></param>
    /// <returns></returns>
    public CbRecord Deserialize(ReadOnlyMemory<byte> buffer)
    {
        return _deserializer.Exec(buffer);
    }

    public byte[] Serialize(CbRecord record)
    {
        ArgumentNullException.ThrowIfNull(record);

        return _serializer.Exec(record);
    }
}
