using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.PictureClause.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class LeafNode(
    string name, int offset = 0, int storageOccupied = 0, int? index = null
) : StorageNode(name, offset, storageOccupied, index)
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

    public override void SetAlias(IStorageNode alias)
    {
        throw new NotSupportedException("Leaf nodes cannot be aliased.");
    }

    // ----------------------------
    // PICTURE Clause
    // ----------------------------

    public PicMeta? Pic { get; private set; } = null;

    public void SetPicMeta(PicMeta pic) => Pic = pic;


    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent)
    {
        writer.WriteLine($"{Indent(indent)}{Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");
    }
}