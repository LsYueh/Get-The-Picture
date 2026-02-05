using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class GroupNode(
    string name, int offset = 0, int? index = null
) : StorageNode(name, offset, null, index)
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
        writer.WriteLine($"{Indent(indent)}{Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");

        foreach (var child in Children)
        {
            child.Dump(writer, indent + 1);
        }
    }
}