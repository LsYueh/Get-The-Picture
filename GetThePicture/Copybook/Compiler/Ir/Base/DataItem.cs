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

    // ----------------------------
    // Union Buffer
    // ----------------------------

    public int Offset { get; private set; }

    /// <summary>
    /// 設定 offset，用於 DFS 遍歷計算 union buffer
    /// </summary>
    internal void SetOffset(int offset)
    {
        Offset = offset;
    }

    public int StorageOccupied { get; internal set; }

    public abstract void CalculateStorage();

    // ----------------------------
    // Dump
    // ----------------------------

    public abstract void Dump(TextWriter writer, int indent = 0);

    private protected static string Indent(int i) => new(' ', i * 2);
}
