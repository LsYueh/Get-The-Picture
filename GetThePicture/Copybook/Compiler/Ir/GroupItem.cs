namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class GroupItem : IBaseItem
{
    public int Level { get; init; }
    public string Name { get; init; } = "";
    public int? Occurs { get; init; }

    public List<IBaseItem> Children { get; } = [];
}
