namespace GetThePicture.Forge.Commands.Wrapper.Base;

public sealed class PathSegment(string name)
{
    public string Name { get; } = name ?? throw new ArgumentNullException(nameof(name));

    // 支援多維 OCCURS (C(1,2)) 顯示
    private readonly List<int> _indices = new();

    public IReadOnlyList<int> Indices => _indices;

    public PathSegment(string name, int index)
        : this(name)
    {
        _indices.Add(index);
    }

    public void AddIndex(int index)
    {
        _indices.Add(index);
    }

    public override string ToString()
    {
        if (_indices.Count == 0)
            return Name;

        return $"{Name}({string.Join(",", _indices)})";
    }
}
