using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Provider;
using GetThePicture.Copybook.SerDes.Record;

namespace Copycat.Core;

public class Deserializer
{
    private DataProvider? _provider = null;
    private CbSerDes? _serDes = null;

    /// <summary>
    /// 
    /// </summary>
    /// <param name="streamReader">Copybook layout source</param>
    public Deserializer(StreamReader streamReader) => Init(streamReader);

    /// <summary>
    /// 
    /// </summary>
    /// <param name="streamReader">Copybook layout source</param>
    /// <returns></returns>
    public CbLayout Init (StreamReader streamReader)
    {
        _provider = new DataProvider(streamReader);
        _serDes = new CbSerDes(_provider);

        return _provider.GetLayout();
    }
    
    public CbRecord? Exec(ReadOnlyMemory<byte> buffer)
    {
        return _serDes?.Deserialize(buffer);
    }
}