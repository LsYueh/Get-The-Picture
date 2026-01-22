using GetThePicture.Copybook.Compiler;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class ParserTest
{
    [TestMethod]
    public void Semantic_Analysis_Test_01()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10) VALUE 'ABC'.";
        int lineNumber = 1;

        var tokens = Lexer.Tokenize(line, lineNumber).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }
}