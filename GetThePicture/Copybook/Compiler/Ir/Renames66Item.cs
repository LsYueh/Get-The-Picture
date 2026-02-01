using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Renames66Item(
    string name, string from, string? through, string? comment = null
) : DataItem(66, name, null, comment)
{
    public string From { get; init; } = from;
    public string? Thru { get; init; } = through;

    /// <summary>
    /// 被 RENAMES 覆蓋的 Elementary Data Item 名稱（依 Copybook 展開後順序）
    /// </summary>
    public IReadOnlyList<string> AffectedItems { get; private set; } = [];

    public void SetAffectedItems(IEnumerable<string> items)
    {
        AffectedItems = [.. items];
    }

    public override void Dump(TextWriter writer, int indent = 0)
    {
        writer.Write($"{Indent(indent)}66 {Name} >> Renames {From}");

        if (!string.IsNullOrEmpty(Thru))
        {
            writer.Write(" through ");
            writer.Write(Thru);
        }

        if (AffectedItems.Count > 0)
        {
            writer.Write($" ({AffectedItems.Count} items)");
        }

        writer.WriteLine();
    }
}