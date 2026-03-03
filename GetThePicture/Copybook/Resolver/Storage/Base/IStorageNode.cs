namespace GetThePicture.Copybook.Resolver.Storage.Base;

public interface IStorageNode
{
    int Level { get; }

    /// <summary>
    /// Tree/節點名稱
    /// </summary>
    string Name { get; }

    /// <summary>
    /// OCCURS index
    /// </summary>
    int? Index { get; }

    /// <summary>
    /// Floating Comments in Copybook
    /// </summary>
    string? Info { get; }

    /// <summary>
    /// Indicates whether this node represents a filler field (FILLER).
    /// </summary>
    bool Ignored { get; }

    // ----------------------------
    // Alias and Offset (For implementing REDEFINES)
    // ----------------------------

    /// <summary>
    /// Storage alias indicates that this node does not define its own
    /// starting offset, but reuses the offset of another storage node.
    /// The occupied length is still defined by this node.
    /// </summary>
    StorageAlias? Alias { get; }

    bool IsAlias => Alias is not null;
    
    int Offset { get; }

    // ----------------------------
    // Storage Occupied
    // ----------------------------
    
    int? StorageOccupied { get; }

    // ----------------------------
    // Node Operations 
    // ----------------------------

    /// <summary>
    /// Child storage nodes in resolved layout order.
    /// </summary>
    IReadOnlyList<IStorageNode> Children { get; }

    // ----------------------------
    // Dump
    // ----------------------------

    void Dump(TextWriter writer, int indent = 0);
}
