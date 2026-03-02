using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Codec.Semantic;
using GetThePicture.Copybook.Resolver.Storage.Base;

namespace GetThePicture.Copybook.Resolver.Storage.Node;

public class LeafNode(
    int level, string name, PicMeta pic, int offset = 0, int storageOccupied = 0, int? index = null
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

    public PicMeta Pic { get; } = pic;

    public bool IsRenames66 { get; private set;} = false;

    public void SetSemantic(PicSemantic semantic)
    {
        Pic.Semantic = semantic;

        EnsureConsistency();
    }

    private void EnsureConsistency()
    {
        var rule = Rules.GetConstraint(Pic.Semantic);

        if (!rule.IsStructureValid(Pic))
            throw new InvalidOperationException($"Semantic '{Pic.Semantic}' is not compatible with PIC structure '{Pic}'.");
    }

    public void AsRenames()
    {
        IsRenames66 = true;
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent)
    {
        string name = IsRenames66 ? $"*{Name}" : Name;

        writer.WriteLine($"{Indent(indent)}{Level:D2} {name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");
    }
}