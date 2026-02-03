using System.Text;
using GetThePicture.Copybook.Compiler.Ir.Base;
using GetThePicture.PictureClause.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class ElementaryDataItem(
    int level, string name, PicMeta pic, int? occurs = null,
    string? value = null,
    bool isFiller = false, string? comment = null) : DataItem(level, name, occurs, comment)
{
    public PicMeta Pic { get; init; } = pic ?? throw new ArgumentNullException(nameof(pic));

    public bool IsFiller { get; init; } = isFiller;

    public override IReadOnlyList<IDataItem> Children => _conditions;

    /// <summary>
    /// (Obsolete)
    /// </summary>
    public string? Value { get; init; } = value;

    // ----------------------------
    // Union Buffer
    // ----------------------------

    public override void CalculateStorage()
    {
        StorageOccupied = Pic.StorageOccupied * (Occurs ?? 1);
    }

    // ----------------------------
    // Level 88 Condition-name
    // ----------------------------    

    private readonly List<Condition88Item> _conditions = [];
    public IReadOnlyList<Condition88Item> Conditions => _conditions;

    internal void AddCondition(Condition88Item condition)
    {
        _conditions.Add(condition);
    }

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter w, int indent = 0)
    {        
        w.Write($"{Indent(indent)}{Level} {Name}{FormatComment()} >>");

        if (Pic != null)
            w.Write($" PIC: {Pic}");

        if (Occurs is > 1)
            w.Write($" OCCURS: {Occurs}");

        if (Value != null)
            w.Write($" VALUE: \"{Value}\"");

        w.WriteLine();

        foreach (var c in _conditions) c.Dump(w, indent + 1);
    }

    private string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";
}
