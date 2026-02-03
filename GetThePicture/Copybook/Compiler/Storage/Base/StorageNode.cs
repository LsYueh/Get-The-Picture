using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.Compiler.Storage.Base;

public abstract class StorageNode(
    string name, int offset, int storageOccupied
) : IStorageNode
{
    /// <summary>
    /// Tree/節點名稱
    /// </summary>
    public string Name { get; private init; } = name;

    public int Offset { get; private init; } = offset;

    public int StorageOccupied { get; private init; } = storageOccupied;

    public virtual IReadOnlyList<IStorageNode> Children => [];

    public ElementaryDataItem? Item { get; init; } = null;

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
        // Semantic/欄位名稱
        var semanticName = Item?.Name ?? "(group)";
        writer.WriteLine($"{Indent(indent)}{Name} [{semanticName}] offset={Offset} len={StorageOccupied}");
    }
}
