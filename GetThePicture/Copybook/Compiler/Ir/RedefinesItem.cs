using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class RedefinesItem(
    int level, string name, string targetName,
    string? comment = null) : DataItem(level, name, null, comment)
{
    // ----------------------------
    // IDataItem
    // ----------------------------

    private readonly List<ElementaryDataItem> _children = [];
    public override IReadOnlyList<IDataItem> Children => _children;

    internal void AddSubordinate(ElementaryDataItem subordinate)
    {
        ArgumentNullException.ThrowIfNull(subordinate);

        _children.Add(subordinate);
    }
    
    // ----------------------------
    // REDEFINES
    // ----------------------------
    
    public string TargetName { get; init; } = targetName;
    public IDataItem Target { get; private set; } = null!;

    public void SetTarget(IDataItem target) => Target = target;

    // ----------------------------
    // (Obsolete)
    // ----------------------------

    public int StorageOccupied { get; private set; }

    public void CalculateStorage()
    {
        int total = 0;

        foreach (var item in _children)
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