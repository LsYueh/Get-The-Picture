using GetThePicture.Copybook.Resolver.Storage;
using GetThePicture.Copybook.Resolver.Storage.Base;
using GetThePicture.Copybook.Resolver.Storage.Node;

namespace GetThePicture.Forge.Commands.Wrapper.Utils;

public class SingleLevelOne
{
    /// <summary>
    /// Try to retrieve the Level 1 node file name from the storage. <br/>
    /// <br/>
    /// Rules: <br/>
    /// - Only succeeds when Level 1 node count is 1. <br/>
    /// - If more than 2 Level 1 nodes exist, the operation is considered invalid. <br/>
    /// - Returns the name of the first Level 1 node when valid. <br/>
    /// <br/>
    /// Design Notes: <br/>
    /// - Early exit is applied when Level 1 node count exceeds 1. <br/>
    /// - Avoids full dictionary traversal when possible. <br/>
    /// </summary>
    public static bool TryResolve(IStorageNode storage, out string? name)
    {
        ArgumentNullException.ThrowIfNull(storage);

        int count = 0;
        IStorageNode? level1 = null;

        bool stopTraversal = false;

        void WalkLevel1(IStorageNode node)
        {
            if (stopTraversal) return;
            
            if (node.Level == 1)
            {
                count++;

                if (count == 1)
                    level1 = node;

                if (count > 1)
                {
                    stopTraversal = true;
                    return;
                }
            }

            switch (node)
            {
                case CbStorage root:
                    foreach (var child in root.Children)
                        WalkLevel1(child);
                    break;

                case GroupNode group:
                    foreach (var child in group.Children)
                        WalkLevel1(child);
                    break;
            }
        }

        WalkLevel1(storage);

        name = (count == 1 && level1 != null) ? level1.Name : null;

        return name != null;
    }
}