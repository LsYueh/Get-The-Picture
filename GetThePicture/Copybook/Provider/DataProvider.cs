using System.Text;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Resolver;
using GetThePicture.Copybook.Resolver.Storage;

namespace GetThePicture.Copybook.Provider;

/// <summary>
/// Provides both the Copybook <see cref="CbLayout"/> and <see cref="CbStorage"/> as runtime objects for serialization and deserialization (SerDes).
/// </summary>
public sealed class DataProvider : IDataProvider
{
    private readonly CbLayout _layout;
    private readonly CbStorage _storage;

    /// <summary>
    /// Initializes a new instance of <see cref="DataProvider"/> using a <see cref="StreamReader"/> that reads a COBOL Copybook.
    /// </summary>
    /// <param name="reader">The <see cref="StreamReader"/> for the Copybook file (e.g., .cpy or .cbl).</param>
    public DataProvider(StreamReader reader)
    {
        _layout  = CbCompiler.FromStreamReader(reader);
        _storage = CbResolver.FromLayout(_layout);
    }

    public DataProvider(string fileName, Encoding encoding)
    {
        using var reader = new StreamReader(fileName, encoding);

        _layout  = CbCompiler.FromStreamReader(reader);
        _storage = CbResolver.FromLayout(_layout);
    }

    /// <summary>
    /// Gets the <see cref="CbLayout"/> object representing the structure of the Copybook.
    /// This object is lazily initialized on first access.
    /// </summary>
    /// <returns>The <see cref="CbLayout"/> instance.</returns>
    public CbLayout GetLayout() => _layout;

    /// <summary>
    /// Gets the <see cref="CbStorage"/> object representing the resolved storage map of the Copybook.
    /// This object depends on <see cref="GetLayout"/> and is lazily initialized on first access.
    /// </summary>
    /// <returns>The <see cref="CbStorage"/> instance.</returns>
    public CbStorage GetStorage() => _storage;
}
