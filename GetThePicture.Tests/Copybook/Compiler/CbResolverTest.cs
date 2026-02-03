using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.PictureClause.Utils;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]
public class CbResolverTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Copybook_Resolver_Test_01()
    {
        const string filePath = @"TestData/sample-cobol-copybook.cpy";
        using var sr = new StreamReader(filePath, cp950);

        CbLayout layout = CbCompiler.FromStreamReader(sr);
        Assert.IsNotNull(layout);

        CbStorage storage = CbResolver.FromLayout(layout);
        Assert.IsNotNull(storage);

        // layout.Dump(Console.Out);
        // storage.Dump(Console.Out);

        // TODO: ...
    }
}