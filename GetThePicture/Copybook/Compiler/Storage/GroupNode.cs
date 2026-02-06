using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class GroupNode(
    string name, int offset = 0, int? index = null
) : StorageNode(name, offset, null, index)
{
    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent)
    {        
        writer.WriteLine($"{Indent(indent)}{Name}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");

        foreach (var child in Children)
        {
            child.Dump(writer, indent + 1);
        }
    }
}