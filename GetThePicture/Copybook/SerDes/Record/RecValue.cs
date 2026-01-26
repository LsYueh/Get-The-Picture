namespace GetThePicture.Copybook.SerDes.Record;

public sealed class RecValue
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
    public static void Print(RecValue record, int indent = 0)
    {
        string indentStr = new(' ', indent);

        foreach (var kvp in record.Fields)
        {
            switch (kvp.Value)
            {
                case RecValue nested:
                    Console.WriteLine($"{indentStr}{kvp.Key}:");
                    Print(nested, indent + 2);
                    break;

                case object[] array:
                    Console.WriteLine($"{indentStr}{kvp.Key}: [");
                    foreach (var item in array)
                    {
                        if (item is RecValue r)
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
