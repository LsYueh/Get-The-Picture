using System.Text.RegularExpressions;

namespace GetThePicture.Forge.Core;

public static partial class NamingHelper
{
    [GeneratedRegex(@"\(([\d,]+)\)")]
    private static partial Regex OccursIndexRegex();

    private static readonly HashSet<string> CSharpKeywords = new(StringComparer.OrdinalIgnoreCase)
    {
        "abstract", "as", "base", "bool", "break", "byte", "case", "catch",
        "char", "checked", "class", "const", "continue", "decimal", "default",
        "delegate", "do", "double", "else", "enum", "event", "explicit", "extern",
        "false", "finally", "fixed", "float", "for", "foreach", "goto", "if",
        "implicit", "in", "int", "interface", "internal", "is", "lock", "long",
        "namespace", "new", "null", "object", "operator", "out", "override",
        "params", "private", "protected", "public", "readonly", "ref", "return",
        "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string",
        "struct", "switch", "this", "throw", "true", "try", "typeof", "uint",
        "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void",
        "volatile", "while"
    };

    public static string ToPascalCase(string cobolName)
    {
        if (string.IsNullOrWhiteSpace(cobolName))
            return cobolName;

        // 1. 以 '-' 或 '_' 分割
        var parts = cobolName.Split('-', '_', StringSplitOptions.RemoveEmptyEntries);

        // 2. 轉 PascalCase
        var pascal = string.Concat(parts.Select(p =>
            char.ToUpperInvariant(p[0]) + p[1..].ToLowerInvariant()));

        // 3. 避免 C# 保留字
        if (CSharpKeywords.Contains(pascal))
            pascal += "_";

        // 4. 替換 OCCURS index，例如 NAME(1,1) >> Name1_1
        pascal = OccursIndexRegex().Replace(pascal, m => {
            return m.Groups[1].Value.Replace(",", "_");
        });

        return pascal;
    }

    public static string ToQualifiedPascalName(string cobolName, string separator = ".")
    {
        if (string.IsNullOrWhiteSpace(cobolName))
            return cobolName;

        // 沒有結構語意，直接回傳
        if (!cobolName.Contains("::", StringComparison.Ordinal))
            return cobolName;

        separator = string.IsNullOrEmpty(separator) ? "." : separator;

        var parts = cobolName
            .Split("::", StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Select(p => EnsureLeadingUpper(p));

        return string.Join(separator, parts);
    }

    private static string EnsureLeadingUpper(string name)
    {
        if (string.IsNullOrEmpty(name))
            return name;

        return char.ToUpperInvariant(name[0]) + name[1..];
    }
}
