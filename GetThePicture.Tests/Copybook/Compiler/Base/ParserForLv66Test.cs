using System.Text;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForLv66Test
{
    private static readonly Lexer lexer = new();

    [TestMethod]
    [DataTestMethod]
    [DataRow("66  EMP-KEY RENAMES EMP-ID.", "COPYBOOK-LAYOUT", "  66 EMP-KEY >> Renames EMP-ID")]
    [DataRow("66  EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.", "COPYBOOK-LAYOUT", "  66 EMP-KEY >> Renames EMP-ID through EMP-DEPT")]
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
        
        var expected = expected_01 + Environment.NewLine + expected_02 + Environment.NewLine;
        Assert.AreEqual(expected, result);
    }
}