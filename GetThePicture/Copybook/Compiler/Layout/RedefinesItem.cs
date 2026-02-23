using GetThePicture.Copybook.Compiler.Layout.Base;

namespace GetThePicture.Copybook.Compiler.Layout;

public sealed class RedefinesItem(
    int level, string name, string targetName,
    string? comment = null) : GroupItem(level, name, null, false, comment)
{
    // ----------------------------
    // REDEFINES
    // ----------------------------
    
    public string TargetName { get; init; } = targetName;
    public IDataItem Target { get; private set; } = null!;

    public void SetTarget(IDataItem target) => Target = target;

    // ----------------------------
    // Dump
    // ----------------------------

    public override void Dump(TextWriter writer, int indent = 0)
    {
        writer.Write($"{Indent(indent)}{Level:D2} {Name} REDEFINES {TargetName}.");
        
        if (!string.IsNullOrWhiteSpace(Comment))
        {
            writer.Write($"  *> {Comment}");
        }

        writer.WriteLine();

        foreach (var child in Children) child.Dump(writer, indent + 1);
    }
}