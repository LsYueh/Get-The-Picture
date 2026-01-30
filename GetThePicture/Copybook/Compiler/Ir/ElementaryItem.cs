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

    public override IReadOnlyList<IDataItem> Children => [];

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
    }

    private string FormatComment() => (Comment != null) ? $" [{Comment}]" : "";

    private static string Indent(int i) => new(' ', i * 2);
}

