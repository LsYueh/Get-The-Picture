using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.Compiler.Storage.Base;

public interface IStorageNode
{
    string Name { get; }
    
    int Offset { get; }
    
    int StorageOccupied { get; }

    IReadOnlyList<IStorageNode> Children { get; }

    ElementaryDataItem? Item { get; }

    // ----------------------------
    // Dump
    // ----------------------------

    void Dump(TextWriter writer, int indent = 0);
}
