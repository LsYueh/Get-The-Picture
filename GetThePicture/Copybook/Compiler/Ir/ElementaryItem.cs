using GetThePicture.Cobol.Picture;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class ElementaryDataItem(
    int level, string name, PicClause pic,
    int? occurs = null, string? value = null,
    bool? isFiller = false) : IDataItem
{
    public int Level { get; init; } = level;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = occurs;
    public string? Value { get; init; } = value;
    public bool? IsFiller { get; init; } = isFiller;

    public PicClause Pic { get; init; } = pic ?? throw new ArgumentNullException(nameof(pic));

    // ----------------------------
    // Dump
    // ----------------------------

    public void Dump(TextWriter w, int indent = 0)
    {
        w.Write($"{Indent(indent)}{Level} {Name} >>");

        if (Pic != null)
            w.Write($" PIC: {Pic}");

        if (Occurs is > 1)
            w.Write($" OCCURS: {Occurs}");

        if (Value != null)
            w.Write($" VALUE: \"{Value}\"");

        w.WriteLine();
    }

    private static string Indent(int i) => new(' ', i * 2);
}

