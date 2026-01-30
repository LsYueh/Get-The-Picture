using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class GroupItem(
    int level, string name, int? occurs = null,
    string? comment = null) : DataItem(level, name, occurs, comment)
{
    private readonly List<IDataItem> _children = [];
    public override IReadOnlyList<IDataItem> Children => _children;

    public int StorageOccupied { get; private set; }

    public void AddSubordinate(IDataItem subordinate)
    {
        ArgumentNullException.ThrowIfNull(subordinate);

        _children.Add(subordinate);
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

    public override void Dump(TextWriter w, int indent = 0)
    {
        w.WriteLine($"{Indent(indent)}{Level} {Name}{FormatOccurs()}{FormatComment()}");

        foreach (var child in _children) child.Dump(w, indent + 1);
    }

    private string FormatOccurs() => Occurs is > 1 ? $" OCCURS {Occurs}" : "";

    private string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";
}
