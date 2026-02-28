using System.Text;

using GetThePicture.Copybook.Provider;
using GetThePicture.Copybook.Resolver.Storage;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

using GetThePicture.Forge.Core;
using GetThePicture.Forge.Core.Config;
using GetThePicture.Forge.Commands.Wrapper.Utils;


namespace GetThePicture.Forge.Commands.Wrapper;

public class WrapperCommand(ForgeConfig config)
{
    private readonly ForgeConfig _config = config;
    
    private Dictionary<string, LeafNode> _map = null!;

    private protected static string Indent(int i) => new(' ', i * 4);

    public void ForgeCode(IDataProvider provider, string fileName)
    {
        var fields = _config.Fields();

        var storage = provider.GetStorage();

        // 如果只有一個 Level 1 的 Group Item，就拿這個 Group Item 來當作 Class 的名稱
        bool haveSingleLevel1 = SingleLevelOne.TryResolve(storage, out string? name);

        if (haveSingleLevel1 && name != null)
        {
            fileName = name;
        }
        
        _map = FlatLeafMap.Build(storage, fields, haveSingleLevel1);
        
        using var w = new StreamWriter($"{fileName}.cs", false, Encoding.UTF8);

        w.WriteLine($"using GetThePicture.Copybook.Wrapper;");
        w.WriteLine($"using GetThePicture.Copybook.Wrapper.Base;");

        if (fields.Values.Any(f => !string.IsNullOrWhiteSpace(f.Type)))
        {
            w.WriteLine("using GetThePicture.Picture.Clause.Base.ClauseItems;");
        }
        
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

        var info = !string.IsNullOrEmpty(node.Info) ? $" // {node.Info}" : $"";

        var semantic = (node.Pic.Semantic != PicSemantic.None) ? $", PicSemantic.{node.Pic.Semantic}" : "";

        // keyName 右補空格，對齊 "="
        string paddedKey = $"[\"{keyName}\"]".PadRight(maxKeyLength + 4); // 4 是額外空格補償

        w.WriteLine($"{indent}{paddedKey} = new CbAddress({node.Offset + 1, 4:D}, {node.StorageOccupied, 3:D}, \"{node.Pic.Raw}\"{semantic}),{info}");

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
}
