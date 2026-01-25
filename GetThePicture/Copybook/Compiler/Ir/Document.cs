namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Document(string name = "COPYBOOK") : IDataItem
{
    public int Level { get; init; } = 0;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = null;
    public string? Comment { get; } = null;

    private readonly List<IDataItem> _dataItems = [];

    public IReadOnlyList<IDataItem> DataItems => _dataItems;
    public int StorageOccupied { get; private set; }

    public void AddSubordinate(IDataItem dataItem)
    {
        ArgumentNullException.ThrowIfNull(dataItem);

        _dataItems.Add(dataItem);
    }

    public void CalculateStorage()
    {
        int total = 0;

        foreach (var dataItem in DataItems)
        {
            if (dataItem is ElementaryDataItem e)
            {
                total += e.Pic.StorageOccupied * (e.Occurs ?? 1);
            }
            else if (dataItem is GroupItem g)
            {
                g.CalculateStorage();
                total += g.StorageOccupied * (g.Occurs ?? 1);
            }
        }

        StorageOccupied = total;
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
