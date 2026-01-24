namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class GroupItem(int level, string name, int? occurs = null) : IDataItem
{
    public int Level { get; init; } = level;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = occurs;

    private readonly List<IDataItem> _subordinates = [];

    public IReadOnlyList<IDataItem> Subordinates => _subordinates;

    public void AddSubordinate(IDataItem subordinate)
    {
        ArgumentNullException.ThrowIfNull(subordinate);

        _subordinates.Add(subordinate);
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public void Dump(TextWriter w, int indent = 0)
    {
        w.WriteLine($"{Indent(indent)}{Level} {Name}{FormatOccurs()}");

        foreach (var subordinate in _subordinates) subordinate.Dump(w, indent + 1);
    }

    private string FormatOccurs() => Occurs is > 1 ? $" OCCURS {Occurs}" : "";

    private static string Indent(int i) => new string(' ', i * 2);    
}
