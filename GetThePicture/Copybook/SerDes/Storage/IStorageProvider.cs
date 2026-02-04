using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;

namespace GetThePicture.Copybook.SerDes.Storage;

public interface IStorageProvider
{
    CbLayout GetLayout();
    CbStorage GetStorage();
}
