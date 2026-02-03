using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Copybook.SerDes.Layout;

public interface ILayoutProvider
{
    CbLayout GetLayout();
}
