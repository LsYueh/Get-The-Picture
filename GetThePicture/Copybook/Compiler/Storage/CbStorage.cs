namespace GetThePicture.Copybook.Compiler.Storage;

/// <summary>
/// Root of Group Nodes
/// </summary>
public sealed class CbStorage() : GroupNode("COPYBOOK-STORAGE-MAP")
{
    // ----------------------------
    // Dump
    // ----------------------------

    // Root node: no offset / length
    public override void Dump(TextWriter w, int indent = 0)
    {
        w.WriteLine($"{Indent(indent)}{Name}");

        foreach (var child in Children) child.Dump(w, indent + 1);
    }
}