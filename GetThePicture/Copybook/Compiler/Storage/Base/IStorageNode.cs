using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.Compiler.Storage.Base;

public interface IStorageNode
{
    string Name { get; }
    
    int Offset { get; }
    
    int? StorageOccupied { get; }

    int? Index { get; }

    IReadOnlyList<IStorageNode> Children { get; }

    // ----------------------------
    // Dump
    // ----------------------------

    void Dump(TextWriter writer, int indent = 0);
}
