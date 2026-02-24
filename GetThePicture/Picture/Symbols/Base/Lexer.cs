namespace GetThePicture.Picture.Symbols.Base;

/// <summary>
/// 詞位
/// </summary>
public static class Lexemes
{
    public const string A = "A";
    public const string X = "X";
    public const string Nine = "9";
    public const string S = "S";
    public const string V = "V";
    public const string Dot = ".";
    public const string P = "P";
    public const string LParen = "(";
    public const string RParen = ")";
}

public class Lexer()
{
    private string _symbols { get; set;} = null!;
    private int _pos = 0;

    private bool IsEnd() => _pos >= _symbols.Length;
    private char Peek() => _symbols[_pos];
    private void Advance() => _pos++;

    public List<Token> Tokenize(string symbols)
    {
        _symbols = symbols ?? throw new ArgumentNullException(nameof(symbols));
        _pos = 0;

        // character-string can contain a maximum of 50 characters.
        if (symbols.Length > 50) 
            throw new ArgumentException("Input exceeds maximum length of 50 characters.", nameof(symbols));
        
        var allTokens = new List<Token>();

        while (!IsEnd())
        {
            char ch = Peek();

            // Ignore whitespace
            if (char.IsWhiteSpace(ch))
            {
                Advance();
                continue;
            }

            int tokenPos = _pos + 1; // 1-based
            var token = ReadToken(ch, tokenPos);
            allTokens.Add(token);
        }

        return allTokens;
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private Token ReadToken(char ch, int tokenPos)
    {
        if (char.IsDigit(ch))
        {
            Advance();
            return new Token(TokenType.Numeric, ch.ToString(), tokenPos);
        }

        switch (ch)
        {
            case 'A': Advance(); return new Token(TokenType.Alphabetic  , Lexemes.A   , tokenPos);
            case 'X': Advance(); return new Token(TokenType.Alphanumeric, Lexemes.X   , tokenPos);

            case 'S': Advance(); return new Token(TokenType.Sign          , Lexemes.S  , tokenPos);
            case 'V': Advance(); return new Token(TokenType.ImpliedDecimal, Lexemes.V  , tokenPos);
            case '.': Advance(); return new Token(TokenType.ImpliedDecimal, Lexemes.Dot, tokenPos);
            case 'P': Advance(); return new Token(TokenType.Scaling       , Lexemes.P  , tokenPos);

            case '(': Advance(); return new Token(TokenType.LParen, Lexemes.LParen, tokenPos);
            case ')': Advance(); return new Token(TokenType.RParen, Lexemes.RParen, tokenPos);

            default:
                Advance();
                return new Token(TokenType.Unknown, ch.ToString(), tokenPos);
        }
    }
}
