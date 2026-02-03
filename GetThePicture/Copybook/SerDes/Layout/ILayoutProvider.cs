using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes.Layout;

public interface ILayoutProvider
{
    CbLayout GetLayout();
}
