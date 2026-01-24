using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook;

[TestClass]

public class ReaderTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";
    private const string expected = "THIS IS A VERY LONG DESCRIPTION THAT NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES";

    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Reader_Test()
    {        
        using var sr = new StreamReader(filePath, cp950);
       
        Document doc = Reader.FromStreamReader(sr);
        Assert.IsNotNull(doc);
        Assert.AreEqual(0, doc.Level);
        Assert.IsNotNull(doc.DataItems);
        Assert.AreEqual(3, doc.DataItems.Count);

        GroupItem? groupItem = (GroupItem?) doc.DataItems[2];
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
    // [Ignore]
    public void Example_Dump_T30_TSE_Demo()
    {
        using var sr = new StreamReader(@"TestData/t30-tse.cpy", cp950);
       
        Document document = Reader.FromStreamReader(sr);

        Assert.IsNotNull(document);

        document.Dump(Console.Out);
    }

    [TestMethod]
    [Ignore]
    public void Example_Dump_T30_OTC_Demo()
    {
        using var sr = new StreamReader(@"TestData/t30-otc.cpy", cp950);
       
        Document document = Reader.FromStreamReader(sr);

        Assert.IsNotNull(document);

        document.Dump(Console.Out);
    }
}