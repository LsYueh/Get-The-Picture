namespace GetThePicture.Copybook.Compiler.Storage.Base;

public abstract class StorageNode(
    string name, int offset = 0, int? storageOccupied = null, int? index = null
) : IStorageNode
{
    public string Name { get; private init; } = name;

    public int? Index { get; } = index;

    public bool Ignored { get; internal set; } = false;

    // ----------------------------
    // Alias and Offset (For implementing REDEFINES)
    // ----------------------------

    public StorageAlias? Alias { get; internal set; } = null;

    public virtual void SetAlias(IStorageNode target)
    {
        // Note: 不知道 COBOL Runtime 本身會不會檢查
        if (StorageOccupied.HasValue && 
            target.StorageOccupied.HasValue &&
            StorageOccupied.Value > target.StorageOccupied.Value)
        {
            throw new InvalidOperationException($"Alias '{Name}' exceeds target storage size.");
        }

        Alias = new StorageAlias(target);
    }

    private readonly int _offset = offset;

    public int Offset => Alias?.Target.Offset ?? _offset;

    // ----------------------------
    // Storage Occupied
    // ----------------------------

    public int? StorageOccupied { get; } = storageOccupied;

    // ----------------------------
    // Node Operations 
    // ----------------------------

    private readonly List<IStorageNode> _children = [];

    public virtual IReadOnlyList<IStorageNode> Children => _children;

    public void AddNode(IStorageNode node)
    {
        ArgumentNullException.ThrowIfNull(node);

        _children.Add(node);
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public abstract void Dump(TextWriter writer, int indent = 0);

    protected void DumpBase(TextWriter writer, int indent)
    {
        writer.WriteLine($"{Indent(indent)}{Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");
    }

    protected static string Indent(int i) => new(' ', i * 2);

    protected string FormatIndex() => Index.HasValue ? $"({Index.Value})" : "";
    
    protected string FormatOffset(bool oneBased = true) => oneBased ? $" start={Offset + 1}" : $" offset={Offset}";

    protected string FormatOccupied(bool showEnd = true)
    {
        if (StorageOccupied is null)
            return "";

        string result = $" len={StorageOccupied}";

        if (showEnd)
        {
            int end = Offset + StorageOccupied.Value + 1;
            result += $" end={end}";
        }

        return result;
    }
}
