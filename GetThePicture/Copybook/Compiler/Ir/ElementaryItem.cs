using GetThePicture.Cobol.Picture;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class ElementaryDataItem : IBaseItem
{
    public int Level { get; init; }
    public string Name { get; init; } = "";
    public int? Occurs { get; init; }

    public PicClause Pic { get; init; } = null!;
}
