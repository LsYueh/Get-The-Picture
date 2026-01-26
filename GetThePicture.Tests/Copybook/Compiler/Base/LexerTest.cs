using GetThePicture.Copybook.Compiler.Base;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

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
        string line = "05 CUSTOMER-NAME PIC X(10) VALUE 'ABC'.";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(10, tokens.Count);

        AssertToken(tokens[0], TokenType.NumericLiteral     , "05");
        AssertToken(tokens[1], TokenType.AlphanumericLiteral, "CUSTOMER-NAME");
        AssertToken(tokens[2], TokenType.Picture            , "PIC");
        AssertToken(tokens[3], TokenType.AlphanumericLiteral, "X");
        AssertToken(tokens[4], TokenType.LParen             , "(");
        AssertToken(tokens[5], TokenType.NumericLiteral     , "10");
        AssertToken(tokens[6], TokenType.RParen             , ")");
        AssertToken(tokens[7], TokenType.Value              , "VALUE");
        AssertToken(tokens[8], TokenType.AlphanumericLiteral, "'ABC'");
        AssertToken(tokens[9], TokenType.Dot                , ".");
    }

    [TestMethod]
    public void Tokenize_Test_02()
    {        
        string line = "05 BGEN-XXXXX  OCCURS 4.";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(5, tokens.Count);

        AssertToken(tokens[0], TokenType.NumericLiteral, "05");
        AssertToken(tokens[1], TokenType.AlphanumericLiteral, "BGEN-XXXXX");
        AssertToken(tokens[2], TokenType.Occurs        , "OCCURS");
        AssertToken(tokens[3], TokenType.NumericLiteral, "4");
        AssertToken(tokens[4], TokenType.Dot           , ".");
    }

    [TestMethod]
    public void Tokenize_Test_03()
    {        
        string line = "07 BGEN-XXXXX-TRANS-NO3     PIC S9(05)V(03) COMP-3.";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(13, tokens.Count);

        AssertToken(tokens[ 0], TokenType.NumericLiteral     , "07");
        AssertToken(tokens[ 1], TokenType.AlphanumericLiteral, "BGEN-XXXXX-TRANS-NO3");
        AssertToken(tokens[ 2], TokenType.Picture            , "PIC");
        AssertToken(tokens[ 3], TokenType.AlphanumericLiteral, "S9");
        AssertToken(tokens[ 4], TokenType.LParen             , "(");
        AssertToken(tokens[ 5], TokenType.NumericLiteral     , "05");
        AssertToken(tokens[ 6], TokenType.RParen             , ")");
        AssertToken(tokens[ 7], TokenType.AlphanumericLiteral, "V");
        AssertToken(tokens[ 8], TokenType.LParen             , "(");
        AssertToken(tokens[ 9], TokenType.NumericLiteral     , "03");
        AssertToken(tokens[10], TokenType.RParen             , ")");
        AssertToken(tokens[11], TokenType.Comp3              , "COMP-3");
        AssertToken(tokens[12], TokenType.Dot                , ".");
    }

    [TestMethod]
    public void Tokenize_Test_04()
    {        
        string line = "VALUE 'O''NEIL'";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(2, tokens.Count);

        AssertToken(tokens[0], TokenType.Value, "VALUE");
        AssertToken(tokens[1], TokenType.AlphanumericLiteral, "'O''NEIL'");
    }

    [TestMethod]
    public void Tokenize_Test_05()
    {        
        string line = "VALUE 'ABC.";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(2, tokens.Count);

        AssertToken(tokens[0], TokenType.Value, "VALUE");
        AssertToken(tokens[1], TokenType.AlphanumericLiteral, "'ABC."); // Note: 缺閉合，不會有Dot
    }

    [TestMethod]
    public void Tokenize_Test_06()
    {        
        string line = "PIC  9(005)";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(5, tokens.Count);

        AssertToken(tokens[0], TokenType.Picture       , "PIC");
        AssertToken(tokens[1], TokenType.NumericLiteral, "9");
        AssertToken(tokens[2], TokenType.LParen        , "(");
        AssertToken(tokens[3], TokenType.NumericLiteral, "005");
        AssertToken(tokens[4], TokenType.RParen        , ")");
    }

    [TestMethod]
    public void Tokenize_Test_07()
    {        
        string line = "01 STOCK-NO PIC X(6). *> 股票代號     ";

        var tokens = lexer.Tokenize(line, 0).ToList();

        Assert.AreEqual(9, tokens.Count);

        AssertToken(tokens[0], TokenType.NumericLiteral     , "01");
        AssertToken(tokens[1], TokenType.AlphanumericLiteral, "STOCK-NO");
        AssertToken(tokens[2], TokenType.Picture            , "PIC");
        AssertToken(tokens[3], TokenType.AlphanumericLiteral, "X");
        AssertToken(tokens[4], TokenType.LParen             , "(");
        AssertToken(tokens[5], TokenType.NumericLiteral     , "6");
        AssertToken(tokens[6], TokenType.RParen             , ")");
        AssertToken(tokens[7], TokenType.Dot                , ".");
        AssertToken(tokens[8], TokenType.Comment            , "股票代號");
    }
}
