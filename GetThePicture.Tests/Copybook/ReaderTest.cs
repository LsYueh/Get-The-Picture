using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook;

[TestClass]

public class ModelBuilderTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";
    private const string expected = "THIS IS A VERY LONG DESCRIPTION THAT NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES";

    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Reader_Test()
    {        
        using var sr = new StreamReader(filePath, cp950);
       
        GroupItem model = Reader.FromStreamReader(sr);
        Assert.IsNotNull(model);
        Assert.AreEqual(1, model.Level);
        Assert.IsNotNull(model.Subordinates);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) model.Subordinates[0];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.IsNotNull(subordinate_05.Pic);
        Assert.IsFalse(subordinate_05.IsFiller);

        Assert.AreEqual(expected, subordinate_05.Value);
    }

    [TestMethod]
    public void Example_Dump_From_Demo()
    {
        using var sr = new StreamReader(@"TestData/demo.cpy", cp950);
       
        GroupItem model = Reader.FromStreamReader(sr);

        model.Dump(Console.Out);
    }
}