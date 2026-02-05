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
        Token _t;

        switch (ch)
        {
            case 'A':
            case 'X':
                _t = new Token(TokenType.Alpha,
                    ch == 'A' ? Lexemes.A : Lexemes.X,
                    tokenPos
                );
                Advance();
                break;

            case '9':
                _t = new Token(TokenType.Numeral, Lexemes.Nine, tokenPos);
                Advance();
                break;

            case 'S':
                _t = new Token(TokenType.Sign, Lexemes.S, tokenPos);
                Advance();
                break;

            case 'V':
            case '.':
                _t = new Token(TokenType.ImpliedDecimal,
                    ch == 'V' ? Lexemes.V : Lexemes.Dot,
                    tokenPos
                );
                Advance();
                break;

            case 'P':
                _t = new Token(TokenType.Scaling, Lexemes.P, tokenPos);
                Advance();
                break;

            case '(':
                _t = new Token(TokenType.LParen, Lexemes.LParen, tokenPos);
                Advance();
                break;

            case ')':
                _t = new Token(TokenType.RParen, Lexemes.RParen, tokenPos);
                Advance();
                break;

            default:
                // Number (repeat count) — only meaningful after '('
                if (char.IsDigit(ch))
                {
                    string number = ReadNumber();
                    _t = new Token(TokenType.Occurs, number, tokenPos);
                }
                else
                {
                    _t = new Token(TokenType.Unknown, ch.ToString(), tokenPos);
                    Advance();
                }
                break;
        }

        return _t;
    }

    private string ReadNumber()
    {
        int start = _pos;
        
        while (!IsEnd() && char.IsDigit(Peek()))
        {
            Advance();
        }

        return _symbols[start.._pos];
    }
}
