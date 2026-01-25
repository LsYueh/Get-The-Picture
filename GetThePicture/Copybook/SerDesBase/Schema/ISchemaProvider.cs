using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes.Schema;

public interface ISchemaProvider
{
    Document GetSchema();
}
