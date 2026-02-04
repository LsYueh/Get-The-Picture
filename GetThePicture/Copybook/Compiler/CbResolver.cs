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

        var storage = new CbStorage();

        ResolveGroupNodes(layout, storage, baseOffset: 0);
        
        return storage;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="item"></param>
    /// <param name="node"></param>
    /// <param name="baseOffset">storage parant 的起始位置</param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    private static int ResolveGroupNodes(IDataItem item, GroupNode node, int baseOffset = 0)
    {
        int storageOffset = 0;

        foreach (var child in item.Children)
        {
            int occurs = child.Occurs ?? 1;

            for (int i = 0; i < occurs; i++)
            {
                switch (child)
                {
                    case RedefinesItem r:
                    {
                        var instanceOffset = ResolveRedefinesOffset(r.TargetName, node.Children);

                        var groupNode = new GroupNode(r.Name, instanceOffset);
                        node.AddNode(groupNode);

                        ResolveGroupNodes(r, groupNode, instanceOffset);

                        // REDEFINES does not advance storage offset

                        break;
                    }
                    
                    case GroupItem g:
                    {
                        var instanceOffset = baseOffset + storageOffset;
                        
                        var groupNode = new GroupNode(g.Name, instanceOffset);
                        node.AddNode(groupNode);
                        
                        int groupSize = ResolveGroupNodes(g, groupNode, instanceOffset);

                        storageOffset += groupSize;

                        break;
                    }

                    case ElementaryDataItem e :
                    {
                        int storageOccupied = e.Pic.StorageOccupied;
                        var instanceOffset = baseOffset + storageOffset;

                        var leafNode = new LeafNode(e.Name, instanceOffset, storageOccupied);
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
    private static int ResolveRedefinesOffset(string name, IEnumerable<IStorageNode> nodes)
    {
        foreach (var node in nodes)
        {
            if (node.Name == name) return node.Offset;
        }

        throw new CompileException($"Cannot resolve REDEFINES target '{name}'.");
    }
}
