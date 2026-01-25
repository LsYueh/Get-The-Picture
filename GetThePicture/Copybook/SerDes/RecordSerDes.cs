using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Schema;

namespace GetThePicture.Copybook.SerDes;

public sealed class RecordSerDes(Document schema)
{
    private readonly Document _schema = schema ?? throw new ArgumentNullException(nameof(schema));

    public RecordSerDes(ISchemaProvider provider): this(provider.GetSchema())
    {
    }

    /// <summary>
    /// Deserialize a single record according to Copybook IR (schema).
    /// </summary>
    /// <param name="record"></param>
    /// <returns></returns>
    public RecordValue Deserialize(ReadOnlySpan<byte> record)
    {
        var cursor = new RecordCursor(record);

        return Deserializer.DesDocument(_schema, ref cursor);
    }

    public byte[] Serialize(object value)
    {
        throw new NotImplementedException();
    }
}

internal ref struct RecordCursor
{
    private readonly ReadOnlySpan<byte> _buffer;
    private int _offset;

    public RecordCursor(ReadOnlySpan<byte> buffer)
    {
        _buffer = buffer;
        _offset = 0;
    }

    public ReadOnlySpan<byte> Read(int length)
    {
        var slice = _buffer.Slice(_offset, length);
        _offset += length;
        return slice;
    }
}

public sealed class RecordValue
{
    private readonly Dictionary<string, object?> _fields = [];

    public object? this[string name]
    {
        get => _fields.TryGetValue(name, out var v) ? v : null;
        set => _fields[name] = value;
    }

    public IReadOnlyDictionary<string, object?> Fields => _fields;
}

public static class RecordValuePrinter
{
    public static void Print(RecordValue record, int indent = 0)
    {
        string indentStr = new(' ', indent);

        foreach (var kvp in record.Fields)
        {
            switch (kvp.Value)
            {
                case RecordValue nested:
                    Console.WriteLine($"{indentStr}{kvp.Key}:");
                    Print(nested, indent + 2);
                    break;

                case object[] array:
                    Console.WriteLine($"{indentStr}{kvp.Key}: [");
                    foreach (var item in array)
                    {
                        if (item is RecordValue r)
                            Print(r, indent + 4);
                        else
                            Console.WriteLine(new string(' ', indent + 4) + item);
                    }
                    Console.WriteLine($"{indentStr}]");
                    break;

                default:
                    Console.WriteLine($"{indentStr}{kvp.Key}: {kvp.Value}");
                    break;
            }
        }
    }
}
