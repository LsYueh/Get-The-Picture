using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Record;

namespace Copycat.Core;

public class Deserializer
{
    private CbSchema? _schema = null;
    private CbSerDes? _serDes = null;

    /// <summary>
    /// 
    /// </summary>
    /// <param name="srSchema">Copybook schema source</param>
    public Deserializer(StreamReader srSchema) => Init(srSchema);

    /// <summary>
    /// 
    /// </summary>
    /// <param name="srSchema">Copybook schema source</param>
    /// <returns></returns>
    public CbSchema Init (StreamReader srSchema)
    {
        _schema = CbCompiler.FromStreamReader(srSchema);
        _serDes = new CbSerDes(_schema);

        return _schema;
    }
    
    public CbRecord? Exec(ReadOnlySpan<byte> buffer)
    {
        return _serDes?.Deserialize(buffer);
    }
}