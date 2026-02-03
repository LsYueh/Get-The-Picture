using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class LeafNode(
    string name, int offset, int storageOccupied
) : StorageNode(name, offset, storageOccupied)
{
    // ----------------------------
    // IStorageNode
    // ----------------------------

    private readonly List<IStorageNode> _children = [];
    public override IReadOnlyList<IStorageNode> Children => _children;

    public void AddNode(IStorageNode node)
    {
        ArgumentNullException.ThrowIfNull(node);

        _children.Add(node);
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent)
    {
        base.DumpBase(writer, indent);
    }
}