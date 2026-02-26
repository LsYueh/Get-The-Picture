using GetThePicture.Copybook.Compiler.Layout.Base;

namespace GetThePicture.Copybook.Compiler.Layout;

/// <summary>
/// Root of Group Items
/// </summary>
public sealed class CbLayout() : GroupItem(0, "COPYBOOK-LAYOUT")
{
    private bool _sealed;

    private List<Renames66Item> _renames66Cache = [];
    private List<IDataItem> _flattenCache = [];

    /// <summary>
    /// Freeze semantic layout and build runtime cache.
    /// Must be called after Analyze().
    /// </summary>
    public void Seal()
    {
        if (_sealed) return;

        _renames66Cache.Clear();
        _flattenCache.Clear();

        CalculateStorage();
        
        Build();

        _sealed = true;
    }

    /// <summary>
    /// Build semantic runtime projection.
    /// </summary>
    private void Build()
    {        
        void Walk(IDataItem node)
        {
            switch (node)
            {
                case RedefinesItem r:
                    _flattenCache.Add(r);

                    foreach (var child in r.Children)
                        Walk(child);

                    return;
                
                case GroupItem g:
                    _flattenCache.Add(g);

                    foreach (var child in g.Children)
                        Walk(child);

                    return;

                case ElementaryDataItem e:
                    if (e.IsFiller != true)
                        _flattenCache.Add(e);

                    return;

                case Renames66Item r:
                    _renames66Cache.Add(r);
                    return;
            }

            // Defensive traversal for future IDataItem extension
            foreach (var child in node.Children)
                Walk(child);
        }

        foreach (var child in Children)
            Walk(child);
    }

    /// <summary>
    /// RENAMES 66 semantic collection.
    /// </summary>
    public IReadOnlyList<Renames66Item> GetRenames66() => _renames66Cache;

    /// <summary>
    /// Runtime semantic linear view.
    /// </summary>
    public IReadOnlyList<IDataItem> GetFlatten() => _flattenCache;

    // ----------------------------
    // Dump
    // ----------------------------

    /// <summary>
    /// Diagnostic dump.
    /// </summary>
    /// <param name="w"></param>
    /// <param name="indent"></param>
    /// <exception cref="InvalidOperationException"></exception>
    public override void Dump(TextWriter w, int indent = 0)
    {
        if (!_sealed)
            throw new InvalidOperationException("Layout must be sealed before dump.");
        
        w.WriteLine($"{Indent(indent)}{Name}");

        foreach (var child in Children) child.Dump(w, indent + 1);
    }
}
