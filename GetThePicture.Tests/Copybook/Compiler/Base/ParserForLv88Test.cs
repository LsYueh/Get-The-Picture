using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserForLv88Test
{
    private static readonly Lexer lexer = new();

    [TestMethod]
    [DataTestMethod]
    [DataRow("88 A VALUE 'A'.")]
    [DataRow("88 B VALUES 'A' 'B' 'C'.")]
    [DataRow("88 DIGIT VALUE 1 THROUGH 9.")]
    [DataRow("88 FLAG VALUE ZERO.")]
    [DataRow("88 SPACE-FLAG VALUE SPACE.")]
    public void Test_Set(string line)
    {        
        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);

        // model.Dump(Console.Out);
    }
}