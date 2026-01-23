namespace GetThePicture.Copybook.Compiler.Ir;

/// <summary>
/// IBM Enterprise COBOL for z/OS : <see href="https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=constants-using-data-items-group-items">Using data items and group items</see>
/// </summary>
public interface IDataItem
{
    int Level { get; }
    string Name { get; }
    int? Occurs { get; }
}