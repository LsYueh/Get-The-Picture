using System.Text;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForRedefinesTest
{
    private static readonly Lexer lexer = new();

    [TestMethod]
    [DataTestMethod]
    [DataRow("05  B REDEFINES A.", "COPYBOOK-LAYOUT", "05 B REDEFINES A.")]
    public void Test_Set(string line, string expected_01, string expected_02)
    {        
        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        model.Dump(writer);

        string result = sb.ToString();
        
        StringAssert.Contains(result, expected_01);
        StringAssert.Contains(result, expected_02);
    }
}