using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes.Schema;

/// <summary>
/// Provides Copybook IR as runtime schema for SerDes.
/// </summary>
public sealed class SchemaProvider(StreamReader reader) : ISchemaProvider
{
    private readonly Lazy<CbSchema> _schema = new(() => CbCompiler.FromStreamReader(reader));

    public CbSchema GetSchema() => _schema.Value;
}
