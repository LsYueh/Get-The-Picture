using System.Text;

using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Base;
using GetThePicture.Copybook.Compiler;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class ParserFromCobolLineTest
{
    private const string filePath = @"TestData/sample-cobol-copybook.cpy";

    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    public void Parse_From_Cpy_01()
    {        
        using var reader = new StreamReader(filePath, cp950);

        var lines = CobolLine.FromStreamReader(reader);

        Lexer lexer = new(lines);

        var tokens = lexer.Tokenize();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }
}