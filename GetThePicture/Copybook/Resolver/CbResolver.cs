using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Copybook.Compiler.Layout.Item;
using GetThePicture.Copybook.Resolver.Storage;
using GetThePicture.Copybook.Resolver.Storage.Base;
using GetThePicture.Copybook.Resolver.Storage.Node;

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
                        
                        var groupNode = Builder.BuildGroupNode(g, instanceOffset, occursIndex);
                        node.AddNode(groupNode);
                        
                        int groupSize = ResolveGroupNodes(g, groupNode, instanceOffset);

                        storageOffset += groupSize;

                        break;
                    }

                    case ElementaryDataItem e :
                    {
                        var instanceOffset = baseOffset + storageOffset;
                        int storageOccupied = e.Pic.StorageOccupied;

                        var leafNode = Builder.BuildLeafNode(e, instanceOffset, storageOccupied, occursIndex);
                        node.AddNode(leafNode);
                        
                        storageOffset += storageOccupied;
                        break;
                    }

                    case Renames66Item re:
                    {
                        LeafNode  from = ResolveAliasRecursive(re.From.Name, node);
                        LeafNode? thru = (re.Thru is not null) ? ResolveAliasRecursive(re.Thru.Name, node) : null;

                        var leafNode = Builder.BuildLeafNode(re, from, thru);
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
