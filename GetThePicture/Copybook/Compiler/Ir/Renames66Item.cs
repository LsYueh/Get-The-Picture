using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Renames66Item(
    string name, string from, string? through, string? comment = null
) : DataItem(66, name, null, comment)
{
    public string From { get; init; } = from;
    public string? Thru { get; init; } = through;

    public override void Dump(TextWriter writer, int indent = 0)
    {
        writer.Write($"{Indent(indent)}66 {Name} >> Renames {From}");

        if (!string.IsNullOrEmpty(Thru))
        {
            writer.Write(" through ");
            writer.Write(Thru);
        }

        writer.WriteLine();
    }
}