using System.Text;

using Copycat.Core;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;

namespace Copycat.Commands;

/// <summary>
/// 提供 Copybook Schema 相關的 CLI 功能。 <br />
/// 將已解析的 CbSchema 轉換成對應的 C# 資料模型 (.cs)。 <br />
/// 生成 Copybook 對應的 record struct 或 class。 <br />
/// </summary>
public static class SchemaCmd
{
    private static readonly Encoding CP950 = Core.EncodingResolver.CP950;

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

    public static void CodeGen (CbSchema schema, string fileName = "Out.cs", bool skipRoot = true)
    {
        using var writer = new StreamWriter(fileName, false, Encoding.UTF8);

        // Namespace
        writer.WriteLine($"namespace GeneratedCopybook;");
        writer.WriteLine();

        if (skipRoot)
        {
            // 遍歷第一層的 Children
            foreach (var child in schema.Children)
            {
                string className = NamingHelper.ToPascalCase(child.Name);
                GenerateClass(writer, className, child.Children, 0);
            }
        }
        else
        {
            // 生成 Root class
            GenerateClass(writer, schema.Name, schema.Children, 0);
        }
    }

    private static void GenerateClass(StreamWriter writer, string className, IReadOnlyList<IDataItem> children, int indentLevel = 0)
    {
        string indent = new(' ', indentLevel * 4);

        writer.WriteLine($"{indent}public sealed class {className}");
        writer.WriteLine($"{indent}{{");

        foreach (var child in children)
        {
            string propIndent = new(' ', (indentLevel + 1) * 4);

            string fieldName = NamingHelper.ToPascalCase(child.Name);

            if (!string.IsNullOrEmpty(child.Comment))
            {
                writer.WriteLine($"{propIndent}/// <summary>");
                writer.WriteLine($"{propIndent}/// {child.Comment}");
                writer.WriteLine($"{propIndent}/// </summary>");
            }

            switch (child)
            {
                case GroupItem g:
                    string subClassName = $"{fieldName}_t";
                    GenerateClass(writer, subClassName, g.Children, indentLevel + 1);
                    writer.WriteLine($"{propIndent}public {subClassName} {fieldName} {{ get; set; }} = new();");
                    break;
                case ElementaryDataItem e :
                    if (e.IsFiller!.Value) break;

                    string csType = TypeToCSharp(e.Pic);

                    bool isReferenceType =
                        csType == "string" ||
                        csType.StartsWith("List<");

                    string initializer = isReferenceType
                        ? csType.StartsWith("List<") ? " = new();" : " = null!;"
                        : "";

                    writer.WriteLine($"{propIndent}public {csType} {fieldName} {{ get; set; }}{initializer}");
                    break;
                default:
                    throw new InvalidOperationException($"Unsupported DataItem type: {child.GetType().Name}");
            }
        }

        writer.WriteLine($"{indent}}}");
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
    
    // public static void Exec(FileInfo file, FileInfo data, bool verbose = true)
    // {
    //     var srSchema = new StreamReader(file.FullName, CP950);
        
    //     var deserializer = new Core.Deserializer(srSchema);

    //     foreach (string line in File.ReadLines(data.FullName, CP950))
    //     {
    //         var buffer = CP950.GetBytes(line);

    //         CbRecord? record = deserializer.Exec(buffer);

    //         if (record is null)
    //         {
    //             if (verbose) Console.Error.WriteLine($"[WARN] Skip invalid record: {line}");
    //             continue;
    //         }
            
    //         if (verbose) {
    //             Console.WriteLine("==== Record ====");
    //             record.Print();
    //             Console.WriteLine("================\n");
    //         }
    //     }
    // }
}
 