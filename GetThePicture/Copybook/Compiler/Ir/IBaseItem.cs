namespace GetThePicture.Copybook.Compiler.Ir;

public interface IBaseItem
{
    int Level { get; }
    string Name { get; }
    int? Occurs { get; }
}