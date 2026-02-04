using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler;

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
                        var alias = ResolveAlias(r.TargetName, node.Children, node.Name);

                        var groupNode = new GroupNode(r.Name, -1);
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
                        
                        var groupNode = new GroupNode(g.Name, instanceOffset, occursIndex);
                        node.AddNode(groupNode);
                        
                        int groupSize = ResolveGroupNodes(g, groupNode, instanceOffset);

                        storageOffset += groupSize;

                        break;
                    }

                    case ElementaryDataItem e :
                    {
                        int storageOccupied = e.Pic.StorageOccupied;
                        var instanceOffset = baseOffset + storageOffset;

                        var leafNode = new LeafNode(e.Name, instanceOffset, storageOccupied, occursIndex);
                        leafNode.SetPicMeta(e.Pic);
                        node.AddNode(leafNode);
                        
                        storageOffset += storageOccupied;
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
    /// <param name="nodes"></param>
    /// <exception cref="CompileException"></exception>
    private static IStorageNode ResolveAlias(string name, IEnumerable<IStorageNode> nodes, string parentName)
    {
        // Assumes node names are unique within the same level

        foreach (var node in nodes)
        {
            if (node.Name == name) return node;
        }

        throw new CompileException($"Cannot resolve REDEFINES target '{name}' in group '{parentName}'.");
    }
}
