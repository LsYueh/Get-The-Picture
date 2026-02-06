using System.Text;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForLv88Test
{
    private static readonly Lexer lexer = new();

    [DataTestMethod]
    [DataRow("88 A VALUE 'A'.", "COPYBOOK-LAYOUT", "88 A >> Value(s) in A")]
    [DataRow("88 B VALUES 'A' 'B' 'C'.", $"COPYBOOK-LAYOUT", "88 B >> Value(s) in A B C")]
    [DataRow("88 DIGIT VALUE 1 THROUGH 9.", $"COPYBOOK-LAYOUT", "88 DIGIT >> Value(s) in 1 through 9")]
    // [DataRow("88 FLAG VALUE ZERO.", "")]
    // [DataRow("88 SPACE-FLAG VALUE SPACE.", "")]
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