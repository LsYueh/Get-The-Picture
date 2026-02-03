namespace GetThePicture.Copybook.Compiler.Ir.Base;

/// <summary>
/// IBM Enterprise COBOL for z/OS : <see href="https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=constants-using-data-items-group-items">Using data items and group items</see>
/// </summary>
public interface IDataItem
{
    int Level { get; }
    string Name { get; }
    int? Occurs { get; }
    string? Comment { get; }

    public IReadOnlyList<IDataItem> Children { get; }

    // ----------------------------
    // Union Buffer
    // ----------------------------

    public int Offset { get; }

    public int StorageOccupied { get; }

    // ----------------------------
    // Dump
    // ----------------------------

    void Dump(TextWriter writer, int indent = 0);
}