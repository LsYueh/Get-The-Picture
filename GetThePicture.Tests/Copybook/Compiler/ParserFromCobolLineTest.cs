using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class ParserFromCobolLineTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";
    private const string expected = "THIS IS A VERY LONG DESCRIPTION THAT NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES";

    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Parse_From_Cpy_01()
    {        
        using var reader = new StreamReader(filePath, cp950);
       
        var lines = CobolLine.FromStreamReader(reader);
        Lexer lexer = new(lines);
        
        var tokens = lexer.Tokenize();
        Parser parser = new(tokens);
        
        GroupItem? root = (GroupItem?) parser.Analyze();
        Assert.IsNotNull(root);
        Assert.AreEqual(1, root.Level);
        Assert.IsNotNull(root.Subordinates);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) root.Subordinates[0];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.IsNotNull(subordinate_05.Pic);
        Assert.IsFalse(subordinate_05.IsFiller);

        Assert.AreEqual(expected, subordinate_05.Value);
    }
}