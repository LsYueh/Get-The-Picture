using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class GroupNode(
    string name, int offset = 0, int storageOccupied = 0
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