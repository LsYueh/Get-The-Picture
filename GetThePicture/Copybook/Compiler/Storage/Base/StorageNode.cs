namespace GetThePicture.Copybook.Compiler.Storage.Base;

public abstract class StorageNode(
    string name, int offset = 0, int? storageOccupied = null, int? index = null
) : IStorageNode
{
    private readonly int _offset = offset;
    
    public string Name { get; private init; } = name;

    public StorageAlias? Alias { get; internal set; } = null;

    public int Offset => Alias?.Target.Offset ?? _offset;

    public int? StorageOccupied { get; private init; } = storageOccupied;

    public int? Index { get; } = index;

    public virtual IReadOnlyList<IStorageNode> Children => [];

    public virtual void SetAlias(IStorageNode target)
    {
        if (StorageOccupied.HasValue && 
            target.StorageOccupied.HasValue &&
            StorageOccupied.Value > target.StorageOccupied.Value)
        {
            throw new InvalidOperationException($"Alias '{Name}' exceeds target storage size.");
        }

        Alias = new StorageAlias(target);
    }


    // ----------------------------
    // Dump
    // ----------------------------

    public abstract void Dump(TextWriter writer, int indent = 0);

    private protected static string Indent(int i) => new(' ', i * 2);

    /// <summary>
    /// 
    /// </summary>
    /// <param name="writer"></param>   
    /// <param name="indent"></param>
    protected void DumpBase(TextWriter writer, int indent)
    {
        writer.WriteLine($"{Indent(indent)}{Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");
    }

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
