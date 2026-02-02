using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class RedefinesItem(
    int level, string name, string targetName,
    string? comment = null) : DataItem(level, name, null, comment)
{
    public string TargetName { get; init; } = targetName;
    public GroupItem Target { get; private set; } = null!;

    private readonly List<ElementaryDataItem> _elementaryDataItems = [];
    public IReadOnlyList<ElementaryDataItem> ElementaryDataItems => _elementaryDataItems;

    public override IReadOnlyList<IDataItem> Children => _elementaryDataItems;

    public int StorageOccupied { get; private set; }

    internal void AddSubordinate(ElementaryDataItem subordinate)
    {
        _elementaryDataItems.Add(subordinate);
    }

    public void CalculateStorage()
    {
        int total = 0;

        foreach (var item in ElementaryDataItems)
        {

            total += item.Pic.StorageOccupied * (item.Occurs ?? 1);

        }

        StorageOccupied = total;
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent = 0)
    {
        writer.Write($"{Indent(indent)}{Level:D2} {Name} REDEFINES {TargetName}.");
        
        if (!string.IsNullOrWhiteSpace(Comment))
        {
            writer.Write($"  *> {Comment}");
        }

        writer.WriteLine();

        foreach (var child in Children) child.Dump(writer, indent + 1);
    }
}