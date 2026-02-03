using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Field;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.Copybook.SerDes.Layout;

namespace GetThePicture.Copybook.SerDes;

public sealed class CbSerDes(CbLayout layout)
{
    private readonly CbLayout _layout = layout ?? throw new ArgumentNullException(nameof(layout));

    public CbSerDes(ILayoutProvider provider): this(provider.GetLayout())
    {
    }

    /// <summary>
    /// Deserialize a single record according to Copybook IR (layout).
    /// </summary>
    /// <param name="record"></param>
    /// <returns></returns>
    public CbRecord Deserialize(ReadOnlySpan<byte> record)
    {
        var access = new CbFieldAccessor(record);

        return CbDeserializer.DesLayout(_layout, ref access);
    }

    public byte[] Serialize(CbRecord value)
    {
        return CbSerializer.SerLayout(_layout, value);
    }
}
