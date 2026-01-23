using GetThePicture.Cobol.Picture;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class ElementaryDataItem(int level, string name, PicClause pic, int? occurs = null) : IDataItem
{
    public int Level { get; init; } = level;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = occurs;

    public PicClause Pic { get; init; } = pic ?? throw new ArgumentNullException(nameof(pic));
}

