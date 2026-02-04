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

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        storage.Dump(writer);

        string result = sb.ToString();

        StringAssert.Contains(result, "LONG-DESCRIPTION start=60");
        StringAssert.Contains(result, "DESC-LINE start=60 len=50");
    }

    [TestMethod]
    public void Copybook_Resolver_Test_02()
    {
        const string filePath = @"TestData/copybook-with-redefines.cpy";
        using var sr = new StreamReader(filePath, cp950);

        CbLayout layout = CbCompiler.FromStreamReader(sr);
        Assert.IsNotNull(layout);

        CbStorage storage = CbResolver.FromLayout(layout);
        Assert.IsNotNull(storage);

        // layout.Dump(Console.Out);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        storage.Dump(writer);
        // storage.Dump(Console.Out);

        string result = sb.ToString();

        StringAssert.Contains(result, "B start=5");
        StringAssert.Contains(result, "B-1 start=5 len=2 end=7");
        StringAssert.Contains(result, "B-2 start=7 len=4 end=11");
        StringAssert.Contains(result, "C start=11 len=4 end=15");
    }

    [TestMethod]
    public void Copybook_Resolver_Test_03()
    {
        const string filePath = @"TestData/copybook-with-redefines-group-in-middle.cpy";
        using var sr = new StreamReader(filePath, cp950);

        CbLayout layout = CbCompiler.FromStreamReader(sr);
        Assert.IsNotNull(layout);

        CbStorage storage = CbResolver.FromLayout(layout);
        Assert.IsNotNull(storage);

        // layout.Dump(Console.Out);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        storage.Dump(writer);
        storage.Dump(Console.Out);

        string result = sb.ToString();

        StringAssert.Contains(result, "A start=5");
        StringAssert.Contains(result, "A-1 start=5 len=2 end=7");
        StringAssert.Contains(result, "A-2 start=7 len=4 end=11");
        StringAssert.Contains(result, "B start=5");
        StringAssert.Contains(result, "B-1 start=5");
        StringAssert.Contains(result, "B-1-1 start=5 len=1 end=6");
        StringAssert.Contains(result, "B-1-2 start=6 len=1 end=7");
        StringAssert.Contains(result, "B-2 start=7 len=4 end=11");
        StringAssert.Contains(result, "C start=11");
    }
}