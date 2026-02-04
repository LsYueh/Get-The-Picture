namespace GetThePicture.Copybook.SerDes.Record;

/// <summary>
/// Represents a COBOL record or group at runtime. <br />
/// <br />
/// Each field can be: <br />
/// - a leaf value (e.g., numeric or string), or <br />
/// - a nested <see cref="CbRecord"/> for a group of fields. <br />
/// <br />
/// Fields are stored in a dictionary and can be accessed via the indexer: <br />
/// <code>
/// var record = new CbRecord();
/// record["CUST-ID"] = "12345";
/// record["ORDER-DETAIL"] = new CbRecord();
/// </code>
/// </summary>
public sealed class CbRecord
{
    /// <summary>
    /// Internal storage for fields and nested groups. <br />
    /// Key = field name, Value = leaf value or <see cref="CbRecord"/> for nested groups.
    /// </summary>
    private readonly Dictionary<string, object?> _fields = [];

    /// <summary>
    /// Get or set a field value by name. <br />
    /// If the field does not exist when getting, returns null. <br />
    /// The value can either be: <br />
    /// - a primitive / string (leaf), or <br />
    /// - a <see cref="CbRecord"/> (nested group) <br />
    /// </summary>
    /// <param name="name">The field name.</param>
    /// <returns>The value of the field or nested record.</returns>
    public object? this[string name]
    {
        get => _fields.TryGetValue(name, out var v) ? v : null;
        set => _fields[name] = value;
    }

    /// <summary>
    /// Read-only access to all fields. <br />
    /// Useful for iteration without allowing modification of the dictionary directly.
    /// </summary>
    public IReadOnlyDictionary<string, object?> Fields => _fields;

    /// <summary>
    /// Prints the record and all nested fields/groups to console or logger. <br />
    /// Useful for debugging.
    /// </summary>
    public void Print() => CbRecordPrinter.Print(this);
}
