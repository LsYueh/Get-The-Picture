using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;

namespace GetThePicture.Copybook.SerDes.Provider;

public interface IDataProvider
{
    CbLayout GetLayout();
    CbStorage GetStorage();
}
