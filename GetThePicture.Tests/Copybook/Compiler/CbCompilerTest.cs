using System.Text;

using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.PictureClause.Utils;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class CbCompilerTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Copybook_Compiler_Test_01()
    {        
        const string filePath = @"TestData/sample-cobol-copybook.cpy";
        using var sr = new StreamReader(filePath, cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);
        Assert.IsNotNull(schema);
        Assert.AreEqual(0, schema.Level);
        Assert.IsNotNull(schema.Children);
        Assert.AreEqual(3, schema.Children.Count);

        GroupItem? groupItem = (GroupItem?) schema.Children[2];
        Assert.IsNotNull(groupItem);

        ElementaryDataItem? elementaryDataItem_05 =  (ElementaryDataItem?) groupItem.Children[0];
        Assert.IsNotNull(elementaryDataItem_05);
        Assert.AreEqual(5, elementaryDataItem_05.Level);
        Assert.IsNotNull(elementaryDataItem_05.Pic);
        Assert.IsFalse(elementaryDataItem_05.IsFiller);

        const string expected = "THIS IS A VERY LONG DESCRIPTION THAT NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES";
        Assert.AreEqual(expected, elementaryDataItem_05.Value);

        // schema.Dump(Console.Out);
    }

    [TestMethod]
    public void Copybook_Compiler_Test_02()
    {
        const string filePath = @"TestData/employee-record-with-levle-88.cpy";
        using var sr = new StreamReader(filePath, cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);

        // schema.Dump(Console.Out);
        
        Assert.IsNotNull(schema);
        Assert.AreEqual(0, schema.Level);
        Assert.IsNotNull(schema.Children);
        Assert.AreEqual(1, schema.Children.Count);

        Assert.AreEqual(3, schema.StorageOccupied);

        GroupItem? GROUP_ITEM_01 = (GroupItem?) schema.Children[0];
        Assert.IsNotNull(GROUP_ITEM_01);
        Assert.AreEqual(1, GROUP_ITEM_01.Level);
        Assert.IsNotNull(GROUP_ITEM_01.Children);
        Assert.AreEqual(3, GROUP_ITEM_01.Children.Count);

        foreach (ElementaryDataItem ITEM_05 in GROUP_ITEM_01.Children.Cast<ElementaryDataItem>())
        {
            Assert.AreEqual(5, ITEM_05.Level);
            Assert.IsNotNull(ITEM_05.Children);

            foreach (Condition88Item cond in ITEM_05.Children.Cast<Condition88Item>())
            {
                Assert.AreEqual(88, cond.Level);
                Assert.IsTrue(cond.Values.Count > 0);
            }
        }
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
    public void Example_Schema_Dump_Nested_Occurs_Record()
    {
        using var sr = new StreamReader(@"TestData/nested-occurs-record.cpy", cp950);

        CbSchema schema = CbCompiler.FromStreamReader(sr);

        Assert.IsNotNull(schema);

        schema.Dump(Console.Out);
    }
}