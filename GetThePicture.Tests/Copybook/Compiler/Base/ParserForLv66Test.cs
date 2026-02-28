using System.Text;

using GetThePicture.Cobol.Base;
using GetThePicture.Copybook.Compiler.Base;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForLv66Test
{
    private static readonly Lexer lexer = new();

    [Ignore]
    [DataTestMethod]
    [DataRow(
        "05 EMP-ID PIC X(10)." +
        "66 EMP-KEY RENAMES EMP-ID.",
        "COPYBOOK-LAYOUT", "66 EMP-KEY >> Renames EMP-ID")]
    [DataRow(
        "05 EMP-ID PIC X(10)." +
        "05 EMP-DEPT PIC X(04)." +
        "66 EMP-KEY RENAMES EMP-ID THRU EMP-DEPT.",
        "COPYBOOK-LAYOUT", "  66 EMP-KEY >> Renames EMP-ID through EMP-DEPT")]
    public void Test_Set(string line, string expected_01, string expected_02)
    {        
        var tokens = lexer.Tokenize(line, 1, Area_t.Free).ToList();

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