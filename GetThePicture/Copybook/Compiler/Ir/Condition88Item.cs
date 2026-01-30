using GetThePicture.Copybook.Compiler.Ir.Base;

namespace GetThePicture.Copybook.Compiler.Ir;

public sealed class Condition88Item(
    string name, object? value = null
) : DataItem(88, name)
{
    public object? Value { get; } = value;

    public override void Dump(TextWriter writer, int indent = 0)
    {
        if (Value is null)
            writer.WriteLine($"{Indent(indent)}88 {Name}");
        else
            writer.WriteLine($"{Indent(indent)}88 {Name} VALUE {Value}");
    }
}
