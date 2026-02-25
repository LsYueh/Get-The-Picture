using System.Text;

using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Copybook.Provider;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

using GetThePicture.Forge.Core;
using GetThePicture.Forge.Commands.Wrapper.Base;
using GetThePicture.Forge.Core.Config;

namespace GetThePicture.Forge.Commands.Wrapper;

public class WrapperCommand(ForgeConfig config)
{
    private readonly ForgeConfig _config = config;
    
    private Dictionary<string, LeafNode> _map = null!;

    private protected static string Indent(int i) => new(' ', i * 4);

    public void ForgeCode(IDataProvider provider, string fileName)
    {
        var storage = provider.GetStorage();

        // 如果只有一個 Level 1 的 Group Item，就拿這個 Group Item 來當作 Class 的名稱
        bool haveSingleLevel1 = TryResolveSingleLevel1Name(storage, out string? name);

        if (haveSingleLevel1 && name != null)
        {
            fileName = name;
        }
        
        _map = BuildFlatLeafMap(storage, haveSingleLevel1);
        
        using var w = new StreamWriter($"{fileName}.cs", false, Encoding.UTF8);

        w.WriteLine($"using GetThePicture.Copybook.Wrapper;");
        w.WriteLine($"using GetThePicture.Copybook.Wrapper.Base;");
        w.WriteLine();

        w.WriteLine($"namespace GetThePicture;");
        w.WriteLine();

        ForgeClass(w, fileName);
    }

    private void ForgeClass(StreamWriter w, string className, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}public class {className}_t(byte[]? raw = null) : CbWrapper(raw)");
        w.WriteLine($"{indent}{{");

        ForgeAddressMap(w, indentLevel + 1);

        w.WriteLine();

        ForgeProperties(w, indentLevel + 1);

        w.WriteLine($"{Indent(indentLevel)}}}");
    }

    private void ForgeAddressMap(StreamWriter w, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}// ----------------------------");
        w.WriteLine($"{indent}// Copybook Address Map");
        w.WriteLine($"{indent}// ----------------------------");
        w.WriteLine();

        w.WriteLine($"{indent}protected override Dictionary<string, CbAddress> AddressMap {{ get; }} = new Dictionary<string, CbAddress>");
        w.WriteLine($"{indent}{{");

        int maxKeyLength = _map.Keys.Max(k => k.Length);

        foreach (var kv in _map)
        {
            var keyName = kv.Key;
            var node = kv.Value;
            ForgeAddress(w, keyName, node, maxKeyLength, indentLevel + 1);
        }

        w.WriteLine($"{indent}}};");
    }

    private static void ForgeAddress(StreamWriter w, string keyName, LeafNode node, int maxKeyLength = 0, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        if (node.Pic is null)
            throw new InvalidOperationException($"Leaf node {keyName} does not have PICTURE clause.");

        var info = !string.IsNullOrEmpty(node.Info) ? $" // {node.Info}" : $"";

        // keyName 右補空格，對齊 "="
        string paddedKey = $"[\"{keyName}\"]".PadRight(maxKeyLength + 4); // 4 是額外空格補償

        w.WriteLine($"{indent}{paddedKey} = new CbAddress({node.Offset + 1, 4:D}, {node.StorageOccupied, 3:D}, \"{node.Pic.Raw}\"),{info}");
    }

    private void ForgeProperties(StreamWriter w, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}// ----------------------------");
        w.WriteLine($"{indent}// Strongly Typed Properties");
        w.WriteLine($"{indent}// ----------------------------");
        w.WriteLine();

        bool first = true;

        foreach (var kv in _map)
        {
            var keyName = kv.Key;
            var node = kv.Value;

            if (node.Ignored) continue; // 跳過 FILLER

            if (!first) w.WriteLine();
            
            ForgeProperty(w, keyName, node, indentLevel);

            first = false;
        }
    }

    private static void ForgeProperty(StreamWriter w, string keyName, LeafNode node, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        string propName = NamingHelper.ToQualifiedPascalName(NamingHelper.ToPascalCase(keyName),"_");

        if (node.Pic is null)
            throw new InvalidOperationException($"Leaf node {keyName} does not have PICTURE clause.");

        string clrType = GetClrType(node.Pic);

        ForgePropertySummary(w, node, indentLevel);

        w.WriteLine($"{indent}public {clrType} {propName}");
        w.WriteLine($"{indent}{{");

        ForgePropertyGetSet(w, keyName, clrType, indentLevel + 1);

        w.WriteLine($"{indent}}}");
    }

    private static void ForgePropertySummary(StreamWriter w, LeafNode node, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        if (node.Pic is null)
            throw new InvalidOperationException($"Leaf node {node.Name} does not have PICTURE clause.");

        string occursIndex = (node.Index is > 1) ? $" ({node.Index})" : "";
        
        var namePart = $"{node.Name}{occursIndex}";
        var picPart  = $"{node.Pic.Raw}";
        var prefix   = $"{namePart} {picPart}";

        w.WriteLine($"{indent}/// <summary>");

        if (!string.IsNullOrEmpty(node.Info))
            w.WriteLine($"{indent}/// {prefix} : {node.Info}");
        else
            w.WriteLine($"{indent}/// {prefix}");

        w.WriteLine($"{indent}/// </summary>");
    }

    private static void ForgePropertyGetSet(StreamWriter w, string keyName, string clrType, int indentLevel = 0)
    {
        var indent = Indent(indentLevel);

        w.WriteLine($"{indent}get => this[\"{keyName}\"].Get<{clrType}>();");
        w.WriteLine($"{indent}set => this[\"{keyName}\"].Set(value);");
    }

    /// <summary>
    /// CLR type derived from GetThePicture\Picture\Clause\Decoder\PicDecoder.cs
    /// 用於決定生成強型別屬性對應的 CLR 型別
    /// </summary>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    private static string GetClrType(PicMeta pic)
    {
        Type type = pic.Semantic switch
        {
            PicSemantic.GregorianDate or PicSemantic.MinguoDate => typeof(DateOnly),
            PicSemantic.Time6 or PicSemantic.Time9              => typeof(TimeOnly),
            PicSemantic.Timestamp14                             => typeof(DateTime),
            _ => GetBaseType(pic),
        };

        return SimplifyClrName(type);

        // ----------------------------
        // Lambda Helpers
        // ----------------------------

        Type GetBaseType(PicMeta pic)
        {
            return pic.BaseClass switch
            {
                PicBaseClass.Numeric      => GetNumericType(pic),
                PicBaseClass.Alphanumeric => typeof(string),
                PicBaseClass.Alphabetic   => typeof(string),
                _ => throw new NotSupportedException($"Unsupported PIC Data Type : {pic.BaseClass}"),
            };
        }

        Type GetNumericType(PicMeta pic)
        {
            IMapper mapper = pic.Signed ? new SIntMapper() : new UIntMapper();
            
            var obj = (pic.DecimalDigits != 0) ? 1m : mapper.Map(1m, pic);

            return obj.GetType();
        }

        string SimplifyClrName(Type type) => type switch
        {
            _ when type == typeof(byte)   => "byte",
            _ when type == typeof(sbyte)  => "sbyte",
            _ when type == typeof(short)  => "short",
            _ when type == typeof(ushort) => "ushort",
            _ when type == typeof(int)    => "int",
            _ when type == typeof(uint)   => "uint",
            _ when type == typeof(long)   => "long",
            _ when type == typeof(ulong)  => "ulong",
            _ when type == typeof(float)  => "float",
            _ when type == typeof(double) => "double",
            _ when type == typeof(decimal)=> "decimal",
            _ when type == typeof(bool)   => "bool",
            _ when type == typeof(string) => "string",
            _ when type == typeof(char)   => "char",
            _ => type.Name // 其他保留原名
        };
    }

    private Dictionary<string, LeafNode> BuildFlatLeafMap(IStorageNode node, bool ignoredLevelOne = false)
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
                    // Field Override
                    if (_config.Fields().TryGetValue(leaf.Name, out var field))
                    {
                        // TODO: ...
                        
                        Console.WriteLine($"⚠ Field override applied: {leaf.Name}");
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

    private static string FormatPath(IEnumerable<PathSegment> segments)
    {
        return string.Join("::", segments);
    }

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
    private static bool TryResolveSingleLevel1Name(IStorageNode storage, out string? name)
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
