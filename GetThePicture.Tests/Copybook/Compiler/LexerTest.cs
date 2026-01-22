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
        AssertToken(tokens[1], TokenType.VariableName , "CUSTOMER-NAME");
        AssertToken(tokens[2], TokenType.Picture      , "PIC");
        AssertToken(tokens[3], TokenType.Identifier   , "X");
        AssertToken(tokens[4], TokenType.LParen       , "(");
        AssertToken(tokens[5], TokenType.NumericLiteral, "10");
        AssertToken(tokens[6], TokenType.RParen       , ")");
        AssertToken(tokens[7], TokenType.Value        , "VALUE");
        AssertToken(tokens[8], TokenType.AlphanumericLiteral, "'ABC'");
        AssertToken(tokens[9], TokenType.Dot          , ".");
    }

    [TestMethod]
    public void Tokenize_Test_02()
    {        
        string line = "05 BGEN-XXXXX  OCCURS 4.";
        int lineNumber = 1;

        var tokens = Lexer.Tokenize(line, lineNumber).ToList();

        Assert.AreEqual(5, tokens.Count);

        AssertToken(tokens[0], TokenType.LevelNumber   , "05");
        AssertToken(tokens[1], TokenType.VariableName  , "BGEN-XXXXX");
        AssertToken(tokens[2], TokenType.Occurs        , "OCCURS");
        AssertToken(tokens[3], TokenType.NumericLiteral, "4");
        AssertToken(tokens[4], TokenType.Dot           , ".");
    }

    [TestMethod]
    public void Tokenize_Test_03()
    {        
        string line = "07 BGEN-XXXXX-TRANS-NO3     PIC S9(05)V(03) COMP-3.";
        int lineNumber = 1;

        var tokens = Lexer.Tokenize(line, lineNumber).ToList();

        Assert.AreEqual(13, tokens.Count);

        AssertToken(tokens[ 0], TokenType.LevelNumber   , "07");
        AssertToken(tokens[ 1], TokenType.VariableName  , "BGEN-XXXXX-TRANS-NO3");
        AssertToken(tokens[ 2], TokenType.Picture       , "PIC");
        AssertToken(tokens[ 3], TokenType.Identifier    , "S9");
        AssertToken(tokens[ 4], TokenType.LParen        , "(");
        AssertToken(tokens[ 5], TokenType.NumericLiteral, "05");
        AssertToken(tokens[ 6], TokenType.RParen        , ")");
        AssertToken(tokens[ 7], TokenType.Identifier    , "V");
        AssertToken(tokens[ 8], TokenType.LParen        , "(");
        AssertToken(tokens[ 9], TokenType.NumericLiteral, "03");
        AssertToken(tokens[10], TokenType.RParen        , ")");
        AssertToken(tokens[11], TokenType.Comp3         , "COMP-3");
        AssertToken(tokens[12], TokenType.Dot           , ".");
    }
}
