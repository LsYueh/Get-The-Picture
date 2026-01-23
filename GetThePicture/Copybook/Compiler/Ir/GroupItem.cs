namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class GroupItem(int level, string name, int? occurs = null) : IDataItem
{
    public int Level { get; init; } = level;
    public string Name { get; init; } = name;
    public int? Occurs { get; init; } = occurs;

    private readonly List<IDataItem> _subordinates = [];

    public IReadOnlyList<IDataItem> Subordinates => _subordinates;

    public void AddSubordinate(IDataItem subordinate)
    {
        ArgumentNullException.ThrowIfNull(subordinate);

        _subordinates.Add(subordinate);
    }
}
