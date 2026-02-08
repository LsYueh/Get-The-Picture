namespace Copycat.Core;

public static class NamingHelper
{
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

        // 2. 每個字首大寫
        var pascal = string.Concat(parts.Select(p =>
            char.ToUpperInvariant(p[0]) + p[1..].ToLowerInvariant()));

        // 3. 避免 C# 保留字
        if (CSharpKeywords.Contains(pascal))
            pascal += "_";

        return pascal;
    }
}
