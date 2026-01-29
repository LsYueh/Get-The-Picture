namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class CbSchema(string name = "COPYBOOK-SCHEMA") : IDataItem
{
    public int Level { get; init; } = 0;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = null;
    public string? Comment { get; } = null;

    private readonly List<IDataItem> _children = [];

    public IReadOnlyList<IDataItem> Children => _children;
    public int StorageOccupied { get; private set; }

    public void AddSubordinate(IDataItem dataItem)
    {
        ArgumentNullException.ThrowIfNull(dataItem);

        _children.Add(dataItem);
    }

    public void CalculateStorage()
    {
        int total = 0;

        foreach (var child in Children)
        {
            if (child is ElementaryDataItem e)
            {
                total += e.Pic.StorageOccupied * (e.Occurs ?? 1);
            }
            else if (child is GroupItem g)
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

        foreach (var child in _children) child.Dump(w, indent + 1);
    }

    private static string Indent(int i) => new(' ', i * 2);    
}
