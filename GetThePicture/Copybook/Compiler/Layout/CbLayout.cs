using GetThePicture.Cobol.Base;
using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Copybook.Compiler.Layout.Item;

namespace GetThePicture.Copybook.Compiler.Layout;

/// <summary>
/// Root of Group Items
/// </summary>
public sealed class CbLayout() : GroupItem(Area_t.None, 0, "COPYBOOK-LAYOUT")
{
    private bool _sealed;

    private readonly List<Renames66Item> _renames66 = [];

    /// <summary>
    /// Freeze semantic layout and build runtime cache.
    /// Must be called after Analyze().
    /// </summary>
    public void Seal()
    {
        if (_sealed) return;

        CalculateStorage();
        
        ValidateRenamesReferences();

        _sealed = true;
    }

    private void ValidateRenamesReferences()
    {        
        List<IDataItem> flatten = [];

        _renames66.Clear();

        void Walk(IDataItem node)
        {
            switch (node)
            {
                case Renames66Item re:
                    _renames66.Add(re);
                    return;
                
                case RedefinesItem r:
                    flatten.Add(r);

                    foreach (var child in r.Children)
                        Walk(child);

                    return;
                
                case GroupItem g:
                    flatten.Add(g);

                    foreach (var child in g.Children)
                        Walk(child);

                    return;

                case ElementaryDataItem e:
                    if (e.IsFiller != true)
                        flatten.Add(e);

                    return;
            }

            // Defensive traversal for future IDataItem extension
            foreach (var child in node.Children)
                Walk(child);
        }

        foreach (var child in Children)
            Walk(child);

        foreach (var rename in _renames66)
            rename.SetAffectedItems(flatten);
    }

    /// <summary>
    /// RENAMES 66 collection.
    /// </summary>
    public IReadOnlyList<Renames66Item> GetRenames66() => _renames66;

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
