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
    private static void ResolveGroupNodes(IDataItem item, GroupNode node, int baseOffset = 0)
    {
        foreach (var child in item.Children)
        {
            int occurs = child.Occurs ?? 1;

            for (int i = 0; i < occurs; i++)
            {
                switch (child)
                {
                    case GroupItem g:
                        var groupNode = new GroupNode(g.Name, baseOffset);
                        node.AddNode(groupNode);
                        // 遞迴
                        ResolveGroupNodes(g, groupNode, baseOffset);
                        // TODO: ...
                        break;

                    case ElementaryDataItem e :
                        var leafNode = new LeafNode(e.Name, baseOffset);
                        node.AddNode(leafNode);
                        // TODO: ...
                        break;

                    default:
                        throw new InvalidOperationException($"Unsupported DataItem type: {child.GetType().Name}");
                }
            }
        }
    }
}
