using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Picture.Clause.Utils;
using GetThePicture.TestData;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]
public class CbResolverTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    [TestMethod]
    public void Copybook_Resolver_Test_01()
    {
        string filePath = TestFileProvider.GetPath("sample-cobol-copybook.cpy");
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

        StringAssert.Contains(result, "LONG-DESCRIPTION start=60");
        StringAssert.Contains(result, "DESC-LINE start=60 len=50");
    }

    [TestMethod]
    public void Copybook_Resolver_Test_02()
    {
        string filePath = TestFileProvider.GetPath("copybook-with-redefines.cpy");
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
        string filePath = TestFileProvider.GetPath("copybook-with-redefines-group-in-middle.cpy");
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

    [TestMethod]
    public void Copybook_Resolver_Test_04()
    {
        string filePath = TestFileProvider.GetPath("nested-occurs-record.cpy");
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

        StringAssert.Contains(result, "ORDER-ID start=1 len=10 end=11");
        StringAssert.Contains(result, "ORDER-LINES(1) start=31");
        StringAssert.Contains(result, "ORDER-LINES(2) start=56");
        StringAssert.Contains(result, "LINE-AMOUNTS(1) start=67");
        StringAssert.Contains(result, "AMOUNT start=67 len=7 end=74");
        StringAssert.Contains(result, "LINE-AMOUNTS(2) start=74");
        StringAssert.Contains(result, "AMOUNT start=74 len=7 end=81");
        StringAssert.Contains(result, "ORDER-LINES(3) start=81");
        StringAssert.Contains(result, "AMOUNT start=99 len=7 end=106");
        StringAssert.Contains(result, "TOTAL-AMOUNT start=106 len=9 end=115");
    }

    [TestMethod]
    public void Copybook_Resolver_Test_05_Redefines()
    {
        string filePath = TestFileProvider.GetPath("twse/m05.cpy");
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

        StringAssert.Contains(result, "PD-ID start=1 len=4 end=5");
        StringAssert.Contains(result, "FIELD-DATA start=29 len=126 end=155");

        StringAssert.Contains(result, "COMT-DATA start=29");
        StringAssert.Contains(result, "COMT-VALUE start=29 len=126 end=155");

        StringAssert.Contains(result, "CMEN-DATA start=29");
        StringAssert.Contains(result, "CMEN-VALUE start=29 len=126 end=155");

        StringAssert.Contains(result, "ANCE-DATA start=29");
        StringAssert.Contains(result, "ANNOUNCE-YMD start=29 len=8 end=37");
        StringAssert.Contains(result, "FILLER start=152 len=3 end=155");

        StringAssert.Contains(result, "OBJ-DATA start=29");
        StringAssert.Contains(result, "OBJ-ID start=29 len=6 end=35");
        StringAssert.Contains(result, "FILLER start=62 len=93 end=155");

        StringAssert.Contains(result, "CTRL-DATA start=29");
        StringAssert.Contains(result, "CREATION-S start=29 len=1 end=30");
        StringAssert.Contains(result, "FILLER start=96 len=59 end=155");
    }
}