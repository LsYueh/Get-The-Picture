using GetThePicture.Cobol.Base;

namespace GetThePicture.Copybook.Compiler.Layout.Base;

/// <summary>
/// 核心抽象類別
/// </summary>
public abstract class DataItem(
    Area_t area,
    int level, string name, int? occurs = null, string? comment = null
) : IDataItem
{
    public Area_t Area { get; } = area;
    
    public int Level { get; } = level;
    public string Name { get; } = name;
    public int? Occurs { get; init; } = occurs;
    public string? Comment { get; init; } = comment;

    public virtual IReadOnlyList<IDataItem> Children => [];

    // ----------------------------
    // Dump
    // ----------------------------

    public abstract void Dump(TextWriter writer, int indent = 0);

    protected string Indent(int i)
    {
        return new string(' ', i * 2) + Margin();
    }

    protected string Margin()
    {
        return Area switch
        {
            Area_t.A => "[A] ",
            Area_t.B => "[B] ",
            _ => "",
        };
    }

    protected string FormatOccurs() => Occurs is > 1 ? $" OCCURS {Occurs}" : "";

    protected string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";
}
