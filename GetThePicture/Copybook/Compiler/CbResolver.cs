using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;

namespace GetThePicture.Copybook.Compiler;

public sealed class CbResolver
{
    public static CbStorage FromLayout(CbLayout layout)
    {
        ArgumentNullException.ThrowIfNull(layout);

        CbStorage storage = new();

        return storage;
    }
}
