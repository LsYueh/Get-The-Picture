using GetThePicture.Copybook.Compiler;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]
public class LexerTest
{
    private static void AssertToken(Token token, TokenType type, string text)
    {
        Assert.AreEqual(type, token.Type);
        Assert.AreEqual(text, token.Value);
    }

    [TestMethod]
    public void Tokenize_Test_01()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10) VALUE 'ABC'.";
        int lineNumber = 1;

        var tokens = Lexer.Tokenize(line, lineNumber).ToList();

        Assert.AreEqual(10, tokens.Count);

        AssertToken(tokens[0], TokenType.LevelNumber  , "05");
        AssertToken(tokens[1], TokenType.Identifier   , "CUSTOMER-NAME");
        AssertToken(tokens[2], TokenType.Keyword      , "PIC");
        AssertToken(tokens[3], TokenType.Identifier   , "X");
        AssertToken(tokens[4], TokenType.LParen       , "(");
        AssertToken(tokens[5], TokenType.Number       , "10");
        AssertToken(tokens[6], TokenType.RParen       , ")");
        AssertToken(tokens[7], TokenType.Keyword      , "VALUE");
        AssertToken(tokens[8], TokenType.StringLiteral, "'ABC'");
        AssertToken(tokens[9], TokenType.Dot          , ".");
    }

    [TestMethod]
    public void Tokenize_Test_02()
    {        
        string line = "05 BGEN-XXXXX  OCCURS 4.";
        int lineNumber = 1;

        var tokens = Lexer.Tokenize(line, lineNumber).ToList();

        Assert.AreEqual(5, tokens.Count);

        AssertToken(tokens[0], TokenType.LevelNumber, "05");
        AssertToken(tokens[1], TokenType.Identifier , "BGEN-XXXXX");
        AssertToken(tokens[2], TokenType.Keyword    , "OCCURS");
        AssertToken(tokens[3], TokenType.Number     , "4");
        AssertToken(tokens[4], TokenType.Dot        , ".");
    }
}
