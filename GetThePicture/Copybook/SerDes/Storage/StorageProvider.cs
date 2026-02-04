using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;

namespace GetThePicture.Copybook.SerDes.Storage;

/// <summary>
/// Provides both the Copybook <see cref="CbLayout"/> and <see cref="CbStorage"/> as runtime objects for serialization and deserialization (SerDes).
/// </summary>
public sealed class StorageProvider : IStorageProvider
{
    private readonly Lazy<CbLayout> _layout;
    private readonly Lazy<CbStorage> _storage;

    /// <summary>
    /// Initializes a new instance of <see cref="StorageProvider"/> using a <see cref="StreamReader"/> that reads a COBOL Copybook.
    /// </summary>
    /// <param name="reader">The <see cref="StreamReader"/> for the Copybook file (e.g., .cpy or .cbl).</param>
    public StorageProvider(StreamReader reader)
    {
        _layout  = new Lazy<CbLayout> (() => CbCompiler.FromStreamReader(reader));
        _storage = new Lazy<CbStorage>(() => CbResolver.FromLayout(_layout.Value));
    }

    /// <summary>
    /// Gets the <see cref="CbLayout"/> object representing the structure of the Copybook.
    /// This object is lazily initialized on first access.
    /// </summary>
    /// <returns>The <see cref="CbLayout"/> instance.</returns>
    public CbLayout GetLayout() => _layout.Value;

    /// <summary>
    /// Gets the <see cref="CbStorage"/> object representing the resolved storage map of the Copybook.
    /// This object depends on <see cref="GetLayout"/> and is lazily initialized on first access.
    /// </summary>
    /// <returns>The <see cref="CbStorage"/> instance.</returns>
    public CbStorage GetStorage() => _storage.Value;
}
