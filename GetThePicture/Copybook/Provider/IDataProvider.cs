using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Resolver.Storage;

namespace GetThePicture.Copybook.Provider;

public interface IDataProvider
{
    CbLayout GetLayout();
    CbStorage GetStorage();
}
