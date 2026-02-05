using GetThePicture.Picture.Symbols.Base;

namespace GetThePicture.Tests.Picture.Symbols.Base;

[TestClass]
public class LexerTest
{
    private static void AssertToken(Token token, TokenType type, string text)
    {
        Assert.AreEqual(type, token.Type);
        Assert.AreEqual(text, token.Value);
    }
    
    private static readonly Lexer lexer = new();

    [TestMethod]
    public void Tokenize_Test_01()
    {
        string symbols = "XX";

        var tokens = lexer.Tokenize(symbols);

        Assert.AreEqual(2, tokens.Count);

        AssertToken(tokens[0], TokenType.Alpha , "X");
        AssertToken(tokens[1], TokenType.Alpha , "X");
    }

    [TestMethod]
    public void Tokenize_Test_02()
    {
        string symbols = "X(10)";

        var tokens = lexer.Tokenize(symbols);

        Assert.AreEqual(4, tokens.Count);

        AssertToken(tokens[0], TokenType.Alpha , "X");
        AssertToken(tokens[1], TokenType.LParen, "(");
        AssertToken(tokens[2], TokenType.Occurs, "10");
        AssertToken(tokens[3], TokenType.RParen, ")");
    }

    [TestMethod]
    public void Tokenize_Test_03()
    {
        string symbols = "99";

        var tokens = lexer.Tokenize(symbols);

        Assert.AreEqual(2, tokens.Count);

        AssertToken(tokens[0], TokenType.Numeral, "9");
        AssertToken(tokens[1], TokenType.Numeral, "9");
    }

    [TestMethod]
    public void Tokenize_Test_04()
    {
        string symbols = "9(10)";

        var tokens = lexer.Tokenize(symbols);

        Assert.AreEqual(4, tokens.Count);

        AssertToken(tokens[0], TokenType.Numeral, "9");
        AssertToken(tokens[1], TokenType.LParen , "(");
        AssertToken(tokens[2], TokenType.Occurs , "10");
        AssertToken(tokens[3], TokenType.RParen , ")");
    }
}