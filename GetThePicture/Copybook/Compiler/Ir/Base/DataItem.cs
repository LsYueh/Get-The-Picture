namespace GetThePicture.Copybook.Compiler.Ir.Base;

/// <summary>
/// 核心抽象類別
/// </summary>
public abstract class DataItem(
    int level, string name, int? occurs = null, string? comment = null
) : IDataItem
{
    public int Level { get; } = level;
    public string Name { get; } = name;
    public int? Occurs { get; init; } = occurs;
    public string? Comment { get; init; } = comment;

    public virtual IReadOnlyList<IDataItem> Children => [];

    public abstract void Dump(TextWriter writer, int indent = 0);

    private protected static string Indent(int i) => new(' ', i * 2);
}
