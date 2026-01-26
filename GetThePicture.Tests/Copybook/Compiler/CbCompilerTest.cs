using System.Text;

using GetThePicture.Cobol.Utils;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class CbCompilerTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";
    private const string expected = "THIS IS A VERY LONG DESCRIPTION THAT NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES";

    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Copybook_Compiler_Test()
    {        
        using var sr = new StreamReader(filePath, cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);
        Assert.IsNotNull(schema);
        Assert.AreEqual(0, schema.Level);
        Assert.IsNotNull(schema.DataItems);
        Assert.AreEqual(3, schema.DataItems.Count);

        GroupItem? groupItem = (GroupItem?) schema.DataItems[2];
        Assert.IsNotNull(groupItem);

        ElementaryDataItem? elementaryDataItem_05 =  (ElementaryDataItem?) groupItem.Subordinates[0];
        Assert.IsNotNull(elementaryDataItem_05);
        Assert.AreEqual(5, elementaryDataItem_05.Level);
        Assert.IsNotNull(elementaryDataItem_05.Pic);
        Assert.IsFalse(elementaryDataItem_05.IsFiller);

        Assert.AreEqual(expected, elementaryDataItem_05.Value);

        // doc.Dump(Console.Out);
    }

    [TestMethod]
    [TestCategory("Demo")]
    [Ignore]
    public void Example_Schema_Dump_T30_TSE()
    {
        using var sr = new StreamReader(@"TestData/t30-tse.cpy", cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);

        Assert.IsNotNull(schema);

        schema.Dump(Console.Out);
    }

    [TestMethod]
    [TestCategory("Demo")]
    [Ignore]
    public void Example_Schema_Dump_T30_OTC()
    {
        using var sr = new StreamReader(@"TestData/t30-otc.cpy", cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);

        Assert.IsNotNull(schema);

        schema.Dump(Console.Out);
    }
}