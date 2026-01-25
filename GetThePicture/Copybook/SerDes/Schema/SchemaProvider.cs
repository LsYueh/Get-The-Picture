using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes.Schema;

/// <summary>
/// Provides Copybook IR as runtime schema for SerDes.
/// </summary>
public sealed class SchemaProvider(StreamReader reader) : ISchemaProvider
{
    private readonly Lazy<Document> _schema = new(() => Reader.FromStreamReader(reader));

    public Document GetSchema() => _schema.Value;
}
