using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.Copybook.SerDes.Schema;

namespace GetThePicture.Copybook.SerDes;

public sealed class CbSerDes(CbSchema schema)
{
    private readonly CbSchema _schema = schema ?? throw new ArgumentNullException(nameof(schema));

    public CbSerDes(ISchemaProvider provider): this(provider.GetSchema())
    {
    }

    /// <summary>
    /// Deserialize a single record according to Copybook IR (schema).
    /// </summary>
    /// <param name="record"></param>
    /// <returns></returns>
    public CbRecord Deserialize(ReadOnlySpan<byte> record)
    {
        var cursor = new RecCursor(record);

        return CbDeserializer.DesSchema(_schema, ref cursor);
    }

    public byte[] Serialize(CbRecord value)
    {
        return CbSerializer.SerSchema(_schema, value);
    }
}
