using System.Text;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForLv66Test
{
    private static readonly Lexer lexer = new();

    [DataTestMethod]
    [DataRow("66  EMP-KEY RENAMES EMP-ID.", "COPYBOOK-LAYOUT", "66 EMP-KEY >> Renames EMP-ID")]
    [DataRow("66  EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.", "COPYBOOK-LAYOUT", "  66 EMP-KEY >> Renames EMP-ID through EMP-DEPT")]
    public void Test_Set(string line, string expected_01, string expected_02)
    {        
        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var layout = parser.Analyze();
        layout.Seal();
        
        Assert.IsNotNull(layout);

        var _66 = layout.GetRenames66();
        Assert.AreEqual(1, _66.Count);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        layout.Dump(writer);

        string result = sb.ToString();
        
        StringAssert.Contains(result, expected_01);
        StringAssert.Contains(result, expected_02);
    }
}