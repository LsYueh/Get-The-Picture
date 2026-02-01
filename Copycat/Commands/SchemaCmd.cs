using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.Compiler.Ir.Base;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;

namespace Copycat.Commands;

public sealed class CodeGenOptions
{
    public bool EmitCondition66 { get; init; } = false;
    /// <summary>
    /// 是否產生 COBOL level-88 condition properties
    /// </summary>
    public bool EmitCondition88 { get; init; } = false;
    public bool SkipRoot { get; init; } = true;
}

/// <summary>
/// 提供 Copybook Schema 相關的 CLI 功能。 <br />
/// 將已解析的 CbSchema 轉換成對應的 C# 資料模型 (.cs)。 <br />
/// 生成 Copybook 對應的 record struct 或 class。 <br />
/// </summary>
public class SchemaCmd(CodeGenOptions? options = null)
{
    private static readonly Encoding CP950 = Core.EncodingResolver.CP950;

    private readonly CodeGenOptions _options = options ?? new CodeGenOptions(); // 預設選項

    public static CbSchema ReadSchema(FileInfo fSchema, bool verbose = true)
    {
        using var reader = new StreamReader(fSchema.FullName, CP950);

        CbSchema schema = CbCompiler.FromStreamReader(reader);

        if (verbose) {
            Console.WriteLine("==== SCHEMA ====");
            schema.Dump(Console.Out);
            Console.WriteLine("================\n");
        }

        return schema;
    }

    public void CodeGen(CbSchema schema, string fileName = "Out.cs")
    {
        using var writer = new StreamWriter(fileName, false, Encoding.UTF8);

        // Namespace
        writer.WriteLine($"namespace GeneratedCopybook;");
        writer.WriteLine();

        writer.WriteLine($"/// <summary>");
        writer.WriteLine($"/// Record Size : {schema.StorageOccupied}");
        writer.WriteLine($"/// </summary>");

        if (!_options.SkipRoot) // 生成 Schema root class
        {
            string schemaName = Core.NamingHelper.ToPascalCase(schema.Name);
            GenerateClass(writer, schemaName, schema, 0);
            return;
        }

        // 遍歷 Schema 第一層的 Children
        foreach (var child in schema.Children)
        {
            string className = Core.NamingHelper.ToPascalCase(child.Name);

            switch (child)
            {
                case Renames66Item r:
                    if (_options.EmitCondition66 == true)
                        GenerateRenames66Props(writer, className, r, 0);
                    break;
                
                default:
                    GenerateClass(writer, className, child, 0);
                    break;
            }

            writer.WriteLine("");
        }
    }

    /// <summary>
    /// Generates a C# sealed class definition from a Copybook IDataItem,
    /// recursively emitting nested classes for group items and properties
    /// for elementary items.
    /// </summary>
    private void GenerateClass(StreamWriter writer, string className, IDataItem item, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        writer.WriteLine($"{indent}public sealed class {className}");
        writer.WriteLine($"{indent}{{");

        foreach (var child in item.Children)
        {
            string fieldName = Core.NamingHelper.ToPascalCase(child.Name);

            switch (child)
            {
                case GroupItem g:
                    GenerateNestedClass(writer, fieldName, g, indentLevel + 1);
                    break;
                
                case ElementaryDataItem e:
                    GenerateProps(writer, fieldName, e, indentLevel + 1);
                    break;

                case Renames66Item r:
                    if (_options.EmitCondition66 == true)
                        GenerateRenames66Props(writer, fieldName, r, indentLevel + 1);
                    break;
                
                default:
                    throw new InvalidOperationException($"Unsupported DataItem type: {child.GetType().Name}");
            }

            writer.WriteLine();
        }

        writer.WriteLine($"{indent}}}");
    }

    private void GenerateNestedClass(StreamWriter writer, string className, IDataItem item, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        string nestedClassName = $"{className}_t";

        // 產生 Nested Class 定義
        GenerateClass(writer, nestedClassName, item, indentLevel);

        writer.WriteLine();

        // 註解
        GenerateComment(writer, item, indentLevel);

        int occurs = item.Occurs ?? 1;
        string propName = ResolveName(className);

        // 根據 OCCURS 決定 property 型別
        if (occurs > 1)
        {
            string elementIndent = new(' ', (indentLevel + 1) * 4);

            writer.WriteLine($"{indent}public {nestedClassName}[] {propName} {{ get; }} = [");

            for (int i = 0; i < occurs; i++)
            {
                string comma = (i < occurs - 1) ? "," : "";
                writer.WriteLine($"{elementIndent}new {nestedClassName}(){comma}");
            }
            
            writer.WriteLine($"{indent}];"); 
        }
        else
        {
            writer.WriteLine($"{indent}public {nestedClassName} {propName} {{ get; set; }} = new();");
        }
    }

    private void GenerateProps(StreamWriter writer, string propName, ElementaryDataItem item, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        GenerateComment(writer, item, indentLevel);

        if (item.IsFiller!.Value) return;

        string csType = TypeToCSharp(item.Pic);

        int occurs = item.Occurs ?? 1;

        if (occurs > 1)
        {
            // OCCURS > 1 → 陣列 property
            writer.WriteLine($"{indent}public {csType}[] {propName} {{ get; }} = new {csType}[{occurs}];");
        }
        else
        {
            bool isReferenceType = csType == "string";

            // 單筆 property
            string initializer = isReferenceType ? " = null!;" : "";
            writer.WriteLine($"{indent}public {csType} {propName} {{ get; set; }}{initializer}");
        }

        if (_options.EmitCondition88)
            GenerateCondition88Props(writer, propName, item, indentLevel);
    }

    private static void GenerateRenames66Props(StreamWriter writer, string className, Renames66Item item, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        writer.WriteLine($"{indent}/// <summary>");
        writer.WriteLine($"{indent}/// 66 '{item.Name}' RENAMES logical grouping. <br />");
        writer.WriteLine($"{indent}/// Original range: '{item.From}'" + (string.IsNullOrEmpty(item.Thru) ? "" : $" through '{item.Thru}'") + ". <br />");
        writer.WriteLine($"{indent}/// This item represents semantic grouping only and does not occupy storage. <br />");
        writer.WriteLine($"{indent}/// Affected elementary items: {item.AffectedItems.Count}.");
        writer.WriteLine($"{indent}/// </summary>");

        writer.WriteLine($"{indent}public sealed class {className}66");
        writer.WriteLine($"{indent}{{");

        if (item.AffectedItems.Count == 0)
        {
            writer.WriteLine($"{indent}    // No resolved elementary items.");
            writer.WriteLine($"{indent}    public static readonly string[] Fields = [];");
        }
        else
        {
            writer.WriteLine($"{indent}    public static readonly string[] Fields = [");

            foreach (var name in item.AffectedItems)
            {
                string propName = Core.NamingHelper.ToPascalCase(name);
                writer.WriteLine($"{indent}        \"{propName}\",");
            }

            writer.WriteLine($"{indent}    ];");
        }

        writer.WriteLine($"{indent}}}");
    }

    private static void GenerateCondition88Props(StreamWriter writer, string basePropName, ElementaryDataItem item, int indentLevel = 0)
    {
        if (item.Conditions.Count == 0) return;

        string indent = new(' ', indentLevel * 4);

        string csType = TypeToCSharp(item.Pic);

        bool isArray = (item.Occurs ?? 1) > 1;
        
        foreach (var cond in item.Conditions)
        {
            string condName = Core.NamingHelper.ToPascalCase(cond.Name);

            if (isArray)
            {
                // 提供 IsXXXAt（0-based）
                writer.WriteLine($"{indent}public bool Is{condName}At(int index)");
                writer.WriteLine($"{indent}{{");
                writer.WriteLine($"{indent}    if (index >= {basePropName}.Length)");
                writer.WriteLine($"{indent}        throw new IndexOutOfRangeException();");
                writer.WriteLine($"{indent}    return {Generate88Expression($"{basePropName}[index]", cond, csType)};");
                writer.WriteLine($"{indent}}}");
                }
            else
            {
                // 單值 → IsXXX
                string expr = Generate88Expression(basePropName, cond, csType);
                writer.WriteLine($"{indent}public bool Is{condName} => {expr};");
            }
        }
    }

    private static string Generate88Expression(string propName, Condition88Item cond, string csType)
    {
        // VALUE A B C
        if (cond.ThroughValue == null)
        {
            var comparisons = cond.Values
                .Select(v => $"{propName} == {FormatValue(v, csType)}");

            return string.Join(" || ", comparisons);
        }

        // VALUE A THROUGH Z
        var start = FormatValue(cond.Values[0], csType);
        var end   = FormatValue(cond.ThroughValue, csType);

        return $"{propName} >= {start} && {propName} <= {end}";
    }

    private static string FormatValue(object value, string csType)
    {
        return csType switch
        {
            "string" => $"\"{value}\"",
            _ => value.ToString()!
        };
    }

    private static void GenerateComment(StreamWriter writer, IDataItem item, int indentLevel = 0)
    {
        switch (item)
        {
            case GroupItem g: GenGroupItemComment(writer, g, indentLevel); break;
            case ElementaryDataItem e: GenElementaryDataItemComment(writer, e, indentLevel); break;
        }
    }

    private static void GenGroupItemComment(StreamWriter writer, GroupItem g, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        string occursText = (g.Occurs is > 1) ? $" Occurs: {g.Occurs}" : "";

        if (string.IsNullOrEmpty(g.Comment) && string.IsNullOrEmpty(occursText)) return;

        writer.WriteLine($"{indent}/// <summary>");

        if (!string.IsNullOrEmpty(g.Comment))
            writer.WriteLine($"{indent}/// {g.Comment}{occursText}");
        else
            writer.WriteLine($"{indent}/// {occursText.TrimStart()}");

        writer.WriteLine($"{indent}/// </summary>");
    }

    private static void GenElementaryDataItemComment(StreamWriter writer, ElementaryDataItem e, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        string occursText = (e.Occurs is > 1) ? $" Occurs: {e.Occurs}" : "";

        var storageOccupied = e.Pic.StorageOccupied;
        string prefix = $"{e.Pic.Raw} [{storageOccupied}]";

         writer.WriteLine($"{indent}/// <summary>");

        if (e.IsFiller == true)
        {
            writer.WriteLine($"{indent}/// {prefix} : FILLER{occursText}");
        }
        else if (!string.IsNullOrEmpty(e.Comment))
        {
            writer.WriteLine($"{indent}/// {prefix} : {e.Comment}{occursText}");
        }
        else
        {
            // 沒 Comment、沒 FILLER，也一定要輸出 Pic 內容
            writer.WriteLine($"{indent}/// {prefix}{occursText}");
        }

        writer.WriteLine($"{indent}/// </summary>");
    }

    /// <summary>
    /// 處理一些特別名稱，如: REC-KEY. 可以直接轉成 Key
    /// </summary>
    /// <param name="className"></param>
    /// <returns></returns>
    private static string ResolveName(string className)
    {
        return className switch
        {
            var n when n.EndsWith("Key", StringComparison.OrdinalIgnoreCase) => "Key",
            _ => className
        };
    }

    private static string TypeToCSharp(PicMeta pic)
    {
        switch (pic.BaseClass)
        {
            case PicBaseClass.Numeric:
                // 小數
                if (pic.DecimalDigits != 0) return "decimal";

                // 整數
                int totalDigits = pic.IntegerDigits;
                bool signed = pic.Signed;

                // 根據 PIC 和 value 決定最佳型別

                if (totalDigits <= 2)
                {
                    return (!signed) ? "byte" : "sbyte";
                }

                if (totalDigits <= 4)
                {
                    return (!signed) ? "ushort" : "short";
                }

                if (totalDigits <= 9)
                {
                    return (!signed) ? "uint" : "int";
                }

                if (totalDigits <= 18)
                {
                    return (!signed) ? "ulong" : "long";
                }

                // 超過 18 位數，一律用 decimal
                return "decimal";
            case PicBaseClass.Alphanumeric:
            case PicBaseClass.Alphabetic:
                return "string";
            default:
                throw new NotSupportedException($"Unsupported PIC Data Type [Decode] : {pic.BaseClass}");
        }
    }
}
 