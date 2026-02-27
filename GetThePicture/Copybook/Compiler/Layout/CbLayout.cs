using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Copybook.Compiler.Utils;

namespace GetThePicture.Copybook.Compiler.Layout;

/// <summary>
/// Root of Group Items
/// </summary>
public sealed class CbLayout() : GroupItem(0, "COPYBOOK-LAYOUT")
{
    private bool _sealed;

    private List<IDataItem> _flattenCache = [];
    private List<Renames66Item> _renames66Cache = [];

    /// <summary>
    /// Freeze semantic layout and build runtime cache.
    /// Must be called after Analyze().
    /// </summary>
    public void Seal()
    {
        if (_sealed) return;

        CalculateStorage();
        
        ResolveReferences();

        _sealed = true;
    }

    private void ResolveReferences()
    {        
        _flattenCache.Clear();
        _renames66Cache.Clear();

        void Walk(IDataItem node)
        {
            switch (node)
            {
                case Renames66Item re:
                    _renames66Cache.Add(re);
                    return;
                
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
            }

            // Defensive traversal for future IDataItem extension
            foreach (var child in node.Children)
                Walk(child);
        }

        foreach (var child in Children)
            Walk(child);

        Redefines.SetTargets(this);
        Renames66.SetFrom(this);
    }

    /// <summary>
    /// Linear view.
    /// </summary>
    public IReadOnlyList<IDataItem> GetFlatten() => _flattenCache;

    /// <summary>
    /// RENAMES 66 collection.
    /// </summary>
    public IReadOnlyList<Renames66Item> GetRenames66() => _renames66Cache;

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
