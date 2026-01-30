using GetThePicture.Copybook.Compiler.Ir.Base;
using GetThePicture.PictureClause.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class ElementaryDataItem(
    int level, string name, PicMeta pic,
    int? occurs = null, string? value = null,
    bool? isFiller = false, string? comment = null) : DataItem(level, name, occurs, comment)
{
    public string? Value { get; init; } = value;

    public bool? IsFiller { get; init; } = isFiller;

    public PicMeta Pic { get; init; } = pic ?? throw new ArgumentNullException(nameof(pic));

    private readonly List<Condition88Item> _conditions = [];
    public IReadOnlyList<Condition88Item> Conditions => _conditions;

    public override IReadOnlyList<IDataItem> Children => _conditions;

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

