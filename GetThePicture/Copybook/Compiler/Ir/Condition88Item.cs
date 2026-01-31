using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Condition88Item(
    string name,
    IEnumerable<object>? values = null,
    object? through = null
) : DataItem(88, name)
{
    /// <summary>
    /// 88 VALUE 列表，可以是一個或多個
    /// </summary>
    public IReadOnlyList<object> Values { get; } = values?.ToList() ?? [];

    /// <summary>
    /// 88 VALUE ... THROUGH 的結尾值
    /// </summary>
    public object? ThroughValue { get; } = through;

    public override void Dump(TextWriter writer, int indent = 0)
    {
        if (Values.Count == 0 && ThroughValue is null)
        {
            writer.WriteLine($"{Indent(indent)}88 {Name}");
        }
        else
        {
            string valuePart = string.Join(" ", Values.Select(v => v?.ToString() ?? "NULL"));

            if (ThroughValue != null)
                valuePart += $" through {ThroughValue}";

            writer.WriteLine($"{Indent(indent)}88 {Name} >> Value(s) in {valuePart}");
        }            
    }
}
