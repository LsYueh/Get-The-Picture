using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Copybook.Compiler.Layout.Item;
using GetThePicture.Copybook.Resolver.Storage;
using GetThePicture.Copybook.Resolver.Storage.Base;
using GetThePicture.Copybook.Resolver.Storage.Node;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Resolver;

public sealed class CbResolver
{
    public static CbStorage FromLayout(CbLayout layout)
    {
        ArgumentNullException.ThrowIfNull(layout);

        var storage = new CbStorage(layout.StorageOccupied);

        ResolveGroupNodes(layout, storage, baseOffset: 0);
        
        return storage;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="item"></param>
    /// <param name="node"></param>
    /// <param name="baseOffset">起始位置 (group 開始)</param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    private static int ResolveGroupNodes(IDataItem item, GroupNode node, int baseOffset = 0)
    {
        // 已佔用 storage，只有 GroupItem 和 ElementaryDataItem 推進
        int storageOffset = 0;

        foreach (var child in item.Children)
        {
            int occurs = child.Occurs ?? 1;

            for (int i = 0; i < occurs; i++)
            {
                int? occursIndex = occurs > 1 ? i + 1 : null;
                
                switch (child)
                {   
                    case RedefinesItem r:
                    {
                        var alias = ResolveAlias(r.TargetName, node);

                        var groupNode = new GroupNode(r.Level, r.Name, -1);
                        groupNode.SetAlias(alias);
                        node.AddNode(groupNode);

                        // 會直接用 Alias 的 Offset 作為 baseOffset
                        ResolveGroupNodes(r, groupNode, groupNode.Offset);

                        // REDEFINES does not advance storage offset

                        break;
                    }
                    
                    case GroupItem g:
                    {
                        var instanceOffset = baseOffset + storageOffset;
                        
                        var groupNode = BuildGroupNode(g, instanceOffset, occursIndex);
                        node.AddNode(groupNode);
                        
                        int groupSize = ResolveGroupNodes(g, groupNode, instanceOffset);

                        storageOffset += groupSize;

                        break;
                    }

                    case ElementaryDataItem e :
                    {
                        var instanceOffset = baseOffset + storageOffset;
                        int storageOccupied = e.Pic.StorageOccupied;

                        var leafNode = BuildLeafNode(e, instanceOffset, storageOccupied, occursIndex);
                        node.AddNode(leafNode);
                        
                        storageOffset += storageOccupied;
                        break;
                    }

                    case Renames66Item re:
                    {
                        LeafNode  from = ResolveAliasRecursive(re.From.Name, node);
                        LeafNode? thru = (re.Thru is not null) ? ResolveAliasRecursive(re.Thru.Name, node) : null;

                        var leafNode = BuildLeafNode(re, from, thru);
                        node.AddNode(leafNode);

                        // RENAMES does not advance storage offset

                        break;
                    }

                    default:
                        throw new InvalidOperationException($"Unsupported DataItem type: {child.GetType().Name}");
                }
            }
        }

        return storageOffset;
    }

    private static GroupNode BuildGroupNode(GroupItem g, int instanceOffset, int? occursIndex)
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
    private static LeafNode BuildLeafNode(ElementaryDataItem e, int instanceOffset, int storageOccupied, int? occursIndex)
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
    /// <exception cref="CompileException"></exception>
    private static LeafNode BuildLeafNode(Renames66Item re, LeafNode from, LeafNode? thru)
    {
        if (thru is not null && thru.Offset < from.Offset)
            throw new CompileException("RENAMES THRU must be after FROM.");
        
        int start = from.Offset;
        int end  = (thru is not null)
            ? thru.Offset + thru.Pic.StorageOccupied
            : from.Offset + from.Pic.StorageOccupied;

        int length = end - start;

        if (length <= 0)
            throw new CompileException("Invalid RENAMES range.");

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

    /// <summary>
    /// 找同層的 GroupNode 或 LeafNode
    /// </summary>
    /// <param name="name"></param>
    /// <param name="nodes"></param>
    /// <exception cref="CompileException"></exception>
    private static IStorageNode ResolveAlias(string name, IStorageNode parent)
    {
        // Assumes node names are unique within the same level

        foreach (var node in parent.Children)
        {
            if (node.Name == name) return node;
        }

        throw new CompileException($"Cannot resolve REDEFINES target '{name}' in group '{parent.Name}'.");
    }

    /// <summary>
    /// 遞迴搜尋 IStorageNode（包含所有子節點）
    /// </summary>
    /// <param name="name"></param>
    /// <param name="parent"></param>
    /// <returns></returns>
    /// <exception cref="CompileException"></exception>
    private static LeafNode ResolveAliasRecursive(string name, IStorageNode parent)
    {
        return TryResolveAliasRecursive(name, parent)
            ?? throw new CompileException(
                $"Cannot resolve REDEFINES target '{name}' in group '{parent.Name}'.");
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="name"></param>
    /// <param name="parent"></param>
    /// <returns></returns>
    private static LeafNode? TryResolveAliasRecursive(string name, IStorageNode parent)
    {
        foreach (var node in parent.Children)
        {
            // 檢查自己
            if (node.Name == name && node is LeafNode leafNode)
                return leafNode;

            // 遞迴往下
            LeafNode? found = TryResolveAliasRecursive(name, node);
            if (found is not null)
                return found;
        }

        return null;
    }
}
