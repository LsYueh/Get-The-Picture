using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class LeafNode(
    string name, int offset = 0, int storageOccupied = 0, int? index = null
) : StorageNode(name, offset, storageOccupied, index)
{
    // ----------------------------
    // StorageNode
    // ----------------------------

    public override void SetAlias(IStorageNode alias)
    {
        throw new NotSupportedException("Leaf nodes cannot be aliased.");
    }

    /// <summary>
    /// 標記為 FILLER，可忽略
    /// </summary>
    public void CanIgnore()
    {
        Ignored = true;
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