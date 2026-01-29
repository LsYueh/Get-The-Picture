namespace GetThePicture.Copybook.SerDes.Record;

public sealed class CbRecord
{
    private readonly Dictionary<string, object?> _fields = [];

    public object? this[string name]
    {
        get => _fields.TryGetValue(name, out var v) ? v : null;
        set => _fields[name] = value;
    }

    public IReadOnlyDictionary<string, object?> Fields => _fields;

    public void Print() => CbRecordPrinter.Print(this);
}
