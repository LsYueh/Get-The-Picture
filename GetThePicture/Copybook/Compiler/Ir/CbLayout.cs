namespace GetThePicture.Copybook.Compiler.Ir;

/// <summary>
/// Root of Group Items
/// </summary>
public sealed class CbLayout() : GroupItem(0, "COPYBOOK-LAYOUT")
{
    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter w, int indent = 0)
    {
        w.WriteLine($"{Indent(indent)}{Name}");

        foreach (var child in Children) child.Dump(w, indent + 1);
    }
}
