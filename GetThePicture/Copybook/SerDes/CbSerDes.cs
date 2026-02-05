using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.Copybook.SerDes.Provider;

namespace GetThePicture.Copybook.SerDes;

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
