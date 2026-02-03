using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Record;

namespace Copycat.Core;

public class Deserializer
{
    private CbLayout? _layout = null;
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
        _layout = CbCompiler.FromStreamReader(streamReader);
        _serDes = new CbSerDes(_layout);

        return _layout;
    }
    
    public CbRecord? Exec(ReadOnlySpan<byte> buffer)
    {
        return _serDes?.Deserialize(buffer);
    }
}