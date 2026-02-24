using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler.Storage;

public class GroupNode(
    int level, string name, int offset = 0, int? index = null
) : StorageNode(level, name, offset, null, index)
{
    // ----------------------------
    // IStorageNode
    // ----------------------------
    
    /// <summary>
    /// 若 Group Item 標記為 FILLER，則為不具名的群組項目 (Unnamed Group Item)
    /// </summary>
    public void Unnamed() => Ignored = true;

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent)
    {        
        writer.WriteLine($"{Indent(indent)}{Level:D2} {DisplayName}{FormatIndex()}{FormatOffset()}{FormatOccupied()}");

        foreach (var child in Children)
        {
            child.Dump(writer, indent + 1);
        }
    }

    private string DisplayName => Ignored ? "<Group>" : Name;
}