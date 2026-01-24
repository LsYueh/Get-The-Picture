namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Document(string name = "COPYBOOK") : IDataItem
{
    public int Level { get; init; } = 0;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = null;
    public string? Comment { get; } = null;

    private readonly List<IDataItem> _dataItems = [];

    public IReadOnlyList<IDataItem> DataItems => _dataItems;

    public void AddSubordinate(IDataItem dataItem)
    {
        ArgumentNullException.ThrowIfNull(dataItem);

        _dataItems.Add(dataItem);
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public void Dump(TextWriter w, int indent = 0)
    {
        w.WriteLine($"{Indent(indent)}{Name}");

        foreach (var dataItem in _dataItems) dataItem.Dump(w, indent + 1);
    }

    private static string Indent(int i) => new(' ', i * 2);    
}
