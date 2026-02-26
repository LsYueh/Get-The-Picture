using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Forge.Commands.Wrapper.Base;
using GetThePicture.Forge.Core.Config.Section;

namespace GetThePicture.Forge.Commands.Wrapper.Utils;

public class FlatLeafMap()
{
    /// <summary>
    /// 
    /// </summary>
    /// <param name="node"></param>
    /// <param name="fields">Field Override</param>
    /// <param name="ignoredLevelOne"></param>
    /// <returns></returns>
    /// <exception cref="InvalidOperationException"></exception>
    public static Dictionary<string, LeafNode> Build(
        IStorageNode node,
        IReadOnlyDictionary<string, FieldOverride> fields,
        bool ignoredLevelOne = false)
    {
        var dict = new Dictionary<string, LeafNode>();

        int fillerCount = 0;

        void Walk(IStorageNode node, IReadOnlyList<PathSegment> parentPath, IStorageNode? parentNode = null)
        {
            PathSegment localPath = new(node.Name);
            
            // 如果 parent 是 Unnamed Group Item，子節點要繼承 index
            if (parentNode is GroupNode { Ignored: true, Index: int index })
            {
                localPath.AddIndex(index);
            }
            
            // 如果自己是 OCCURS
            if (node.Index.HasValue)
            {
                localPath.AddIndex(node.Index.Value);
            }

            switch (node)
            {
                case CbStorage root:
                {
                    foreach (var child in root.Children)
                    {
                        // COPYBOOK-STORAGE-MAP 要排除
                        Walk(child, []);
                    }
                    break;
                }

                case GroupNode group:
                {
                    // Level 1 或 Unnamed Group Item 輸出處裡
                    bool ignored = (group.Level == 1 && ignoredLevelOne) || group.Ignored;
                    
                    List<PathSegment> currentPath = (parentPath.Count == 0)
                        ? [localPath]
                        : [.. parentPath, localPath];
                    
                    List<PathSegment> groupPath = ignored ? [.. parentPath] : currentPath;
                                    
                    foreach (var child in group.Children)
                    {
                        Walk(child, groupPath, group);
                    }
                        
                    break;
                }   

                case LeafNode leaf:
                {
                    var key = leaf.Name;
                    
                    // Field Override
                    if (fields.TryGetValue(key, out var field) && field != null)
                    {
                        Console.WriteLine($"⚠ Override detected for field <{key}>");
                        SemanticOverride(leaf, field);
                    }
                    
                    if (leaf.Ignored) // FILLER
                    {
                        string fillerName = $"FILLER{++fillerCount:D2}";
                        dict.Add(fillerName, leaf);
                        return;
                    }

                    List<PathSegment> currentPath = (parentPath.Count == 0)
                        ? [localPath]
                        : [.. parentPath, localPath];

                    string fullPath = FormatPath(currentPath);

                    if (!dict.TryAdd(fullPath, leaf))
                        throw new InvalidOperationException($"Duplicate leaf path: {fullPath}");

                    break;
                }

                default:
                    throw new InvalidOperationException($"Unsupported storage node type: {node.GetType().Name}");
            }
        }

        Walk(node, []);

        return dict;
    }

    private static bool SemanticOverride(LeafNode leaf, FieldOverride field)
    {
        if (string.IsNullOrWhiteSpace(field.Type))
            return false;

        if (!PicSemanticMap.TryResolve(field.Type, out var semantic))
            throw new InvalidOperationException($"Unknown semantic type: {field.Type}");

        if (leaf.Pic.Semantic == semantic)
            return false;
        
        leaf.Pic.Semantic = semantic;
        
        Console.WriteLine($"    Semantic override → {semantic}");

        return true;
    }

    private static string FormatPath(IEnumerable<PathSegment> segments)
    {
        // TODO: Property Name Override
        
        return string.Join("::", segments);
    }
}
