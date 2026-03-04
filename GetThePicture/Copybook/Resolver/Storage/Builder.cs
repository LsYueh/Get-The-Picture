using GetThePicture.Copybook.Compiler.Layout.Item;
using GetThePicture.Copybook.Resolver.Base;
using GetThePicture.Copybook.Resolver.Storage.Node;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Resolver.Storage;

public class Builder
{
    public static GroupNode BuildGroupNode(GroupItem g, int instanceOffset, int? occursIndex)
    {
        var groupNode = new GroupNode(g.Level, g.Name, instanceOffset, occursIndex);

        if (g.IsFiller)
            groupNode.Unnamed();

        return groupNode;
    }

    /// <summary>
    /// Elementary Data Item
    /// </summary>
    /// <param name="e"></param>
    /// <param name="instanceOffset"></param>
    /// <param name="storageOccupied"></param>
    /// <param name="occursIndex"></param>
    /// <returns></returns>
    public static LeafNode BuildLeafNode(ElementaryDataItem e, int instanceOffset, int storageOccupied, int? occursIndex)
    {
        var leafNode = new LeafNode(
            e.Level, e.Name, e.Pic, 
            instanceOffset, storageOccupied, occursIndex
        );
        
        if (e.Comment is not null)
            leafNode.SetInfo(e.Comment);

        if (e.IsFiller)
            leafNode.CanIgnore();

        return leafNode;
    }

    /// <summary>
    /// Lv 66 RENAMES
    /// </summary>
    /// <param name="re"></param>
    /// <param name="from"></param>
    /// <param name="thru"></param>
    /// <returns></returns>
    /// <exception cref="ResolverException"></exception>
    public static LeafNode BuildLeafNode(Renames66Item re, LeafNode from, LeafNode? thru)
    {
        if (thru is not null && thru.Offset < from.Offset)
            throw new ResolverException("RENAMES THRU must be after FROM.");
        
        int start = from.Offset;
        int end  = (thru is not null)
            ? thru.Offset + thru.Pic.StorageOccupied
            : from.Offset + from.Pic.StorageOccupied;

        int length = end - start;

        if (length <= 0)
            throw new ResolverException("Invalid RENAMES range.");

        // 用 PIC X 去呈現 RENAMES 的 raw byte View
        PicMeta pic = PicMeta.Parse($"X({length})");
        
        LeafNode leafNode = new(
            re.Level, re.Name, pic, 
            start, pic.StorageOccupied
        );

        leafNode.AsRenames();

        if (re.Comment is not null)
            leafNode.SetInfo(re.Comment);

        return leafNode;
    }
}
