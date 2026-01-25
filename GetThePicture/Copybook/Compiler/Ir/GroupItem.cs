namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class GroupItem(
    int level, string name, int? occurs = null,
    string? comment = null) : IDataItem
{
    public int Level { get; init; } = level;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = occurs;
    public string? Comment { get; init; } = comment;

    private readonly List<IDataItem> _subordinates = [];
    public int StorageOccupied { get; private set; }

    public IReadOnlyList<IDataItem> Subordinates => _subordinates;

    public void AddSubordinate(IDataItem subordinate)
    {
        ArgumentNullException.ThrowIfNull(subordinate);

        _subordinates.Add(subordinate);
    }

    public void CalculateStorage()
    {
        int total = 0;

        foreach (var subordinate in Subordinates)
        {
            if (subordinate is ElementaryDataItem e)
            {
                total += e.Pic.StorageOccupied * (e.Occurs ?? 1);
            }
            else if (subordinate is GroupItem g)
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
        w.WriteLine($"{Indent(indent)}{Level} {Name}{FormatOccurs()}{FormatComment()}");

        foreach (var subordinate in _subordinates) subordinate.Dump(w, indent + 1);
    }

    private string FormatOccurs() => Occurs is > 1 ? $" OCCURS {Occurs}" : "";

    private string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";

    private static string Indent(int i) => new(' ', i * 2);    
}
