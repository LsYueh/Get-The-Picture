using System.Text;

using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Copybook.SerDes.Provider;

using GetThePicture.Forge.Core;

namespace GetThePicture.Forge.Commands.Warpper;

public class WarpperCommand(WarpperOptions? opts = null)
{
    private readonly WarpperOptions _opts = opts ?? new (); // 預設選項

    private Dictionary<string, LeafNode> _map = null!;

    private protected static string Indent(int i) => new(' ', i * 4);

    public void ForgeCode(IDataProvider provider, string fileName)
    {
        _map = BuildFlatLeafMap(provider.GetStorage());
        
        using var w = new StreamWriter($"{fileName}.cs", false, Encoding.UTF8);

        w.WriteLine($"using GetThePicture.Copybook.Warpper;");
        w.WriteLine($"using GetThePicture.Copybook.Warpper.Base;");
        w.WriteLine();

        w.WriteLine($"namespace GetThePicture;");
        w.WriteLine();

        ForgeClass(w, fileName);
    }

    private void ForgeClass(StreamWriter w, string className, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}public class {className}_t(byte[] raw) : CbWarpper(raw)");
        w.WriteLine($"{indent}{{");

        ForgeAddressMap(w, indentLevel + 1);

        w.WriteLine();

        ForgeProperties(w, indentLevel + 1);

        w.WriteLine($"{Indent(indentLevel)}}}");
    }

    private void ForgeAddressMap(StreamWriter w, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}protected override Dictionary<string, CbAddress> AddressMap {{ get; }} = new Dictionary<string, CbAddress>");
        w.WriteLine($"{indent}{{");

        foreach (var kv in _map)
        {
            var keyName = kv.Key;
            var node = kv.Value;
            ForgeAddress(w, keyName, node, indentLevel + 1);
        }

        w.WriteLine($"{indent}}};");
    }

    private static void ForgeAddress(StreamWriter w, string keyName, LeafNode node, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        if (node.Pic is null)
            throw new InvalidOperationException($"Leaf node {keyName} does not have PICTURE clause.");

        w.WriteLine($"{indent}[\"{keyName}\"] = new CbAddress({node.Offset + 1}, {node.StorageOccupied}, \"{node.Pic.Raw}\"),");
    }

    private void ForgeProperties(StreamWriter w, int indentLevel = 0)
    {
        bool first = true;

        foreach (var kv in _map)
        {
            if (!first) w.WriteLine();

            var keyName = kv.Key;
            var node = kv.Value;
            ForgeProperty(w, keyName, node, indentLevel);

            first = false;
        }
    }

    private static void ForgeProperty(StreamWriter w, string keyName, LeafNode node, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        string propName = NamingHelper.ToQualifiedPascalName(NamingHelper.ToPascalCase(keyName),"_");

        string clrType = "object";

        w.WriteLine($"{indent}public {clrType} {propName}");
        w.WriteLine($"{indent}{{");

        ForgePropertyGetSet(w, keyName, clrType, indentLevel + 1);

        w.WriteLine($"{indent}}}");
    }

    private static void ForgePropertyGetSet(StreamWriter w, string keyName, string clrType, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}get => ({clrType})this[\"{keyName}\"]!;");
        w.WriteLine($"{indent}set => this[\"{keyName}\"] = value;");
    }

    private static Dictionary<string, LeafNode> BuildFlatLeafMap(IStorageNode node)
    {
        var dict = new Dictionary<string, LeafNode>();

        void Walk(IStorageNode n, string parentPath)
        {
            // 如果有 Index (OCCURS)，就加上 (Index)
            string fieldName = n.Index.HasValue ? $"{n.Name}({n.Index.Value})" : n.Name;

            switch (n)
            {
                case CbStorage root:
                {
                    foreach (var child in root.Children)
                    {
                        // Note: 目前不處理 REDEFINES，這邊不適合做靈活變動
                        if (child.IsAlias) continue;
                    
                        // COPYBOOK-STORAGE-MAP 要排除
                        Walk(child, string.Empty);
                    }
                    break;
                }

                case GroupNode group:
                {
                    var groupPath = string.IsNullOrEmpty(parentPath) ? fieldName : $"{parentPath}::{fieldName}";
                    
                    foreach (var child in group.Children)
                    {
                        // Note: 目前不處理 REDEFINES，這邊不適合做靈活變動
                        if (child.IsAlias) continue;
                    
                        Walk(child, groupPath);
                    }
                        
                    break;
                }   

                case LeafNode leaf:
                    var leafPath = string.IsNullOrEmpty(parentPath) ? fieldName : $"{parentPath}::{fieldName}";

                    if (dict.ContainsKey(leafPath))
                        throw new InvalidOperationException($"Duplicate leaf path: {leafPath}");

                    dict.Add(leafPath, leaf);

                    break;

                default:
                    throw new InvalidOperationException($"Unsupported storage node type: {n.GetType().Name}");
            }
        }

        Walk(node, string.Empty);

        return dict;
    }
}
