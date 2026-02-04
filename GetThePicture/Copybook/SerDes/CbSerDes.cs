using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.Copybook.SerDes.Storage;

namespace GetThePicture.Copybook.SerDes;

public sealed class CbSerDes
{
    private readonly CbLayout _layout;
    
    private readonly CbStorage _storage;

    private CbDeserializer _deserializer;

    public CbSerDes(IStorageProvider provider)
    {
        _layout  = provider.GetLayout();
        _storage = provider.GetStorage();

        _deserializer = new(_storage);
    }

    /// <summary>
    /// Deserialize a single record according to Copybook layout.
    /// </summary>
    /// <param name="record"></param>
    /// <returns></returns>
    public CbRecord Deserialize(ReadOnlyMemory<byte> buffer)
    {
        if (_layout.StorageOccupied != buffer.Length)
            throw new InvalidOperationException($"Record size mismatch: layout={_layout.StorageOccupied}, actual={buffer.Length}");
        
        return _deserializer.Exec(buffer);
    }

    public byte[] Serialize(CbRecord value)
    {
        return CbSerializer.SerLayout(_layout, value);
    }
}
