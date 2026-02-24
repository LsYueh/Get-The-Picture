using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class LeafNode(
    int level, string name, int offset = 0, int storageOccupied = 0, int? index = null
) : StorageNode(level, name, offset, storageOccupied, index)
{
    // ----------------------------
    // IStorageNode
    // ----------------------------

    public void SetInfo(string info) => Info = info;

    /// <summary>
    /// 標記為 FILLER，可忽略
    /// </summary>
    public void CanIgnore() => Ignored = true;

    
    // ----------------------------
    // StorageNode
    // ----------------------------

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
        writer.WriteLine($"{Indent(indent)}{Level:D2} {Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");
    }
}