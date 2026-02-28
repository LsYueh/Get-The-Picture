using GetThePicture.Cobol;

namespace GetThePicture.Copybook.Compiler.Base;

public class Lexer(IReadOnlyList<CobolLine>? lines = null)
{
    private readonly IReadOnlyList<CobolLine>? _lines = lines;
    
    /// <summary>
    /// Current Char Index.
    /// </summary>
    private int _pos = 0;
    
    public List<Token> Tokenize()
    {
        if (_lines == null) throw new InvalidOperationException("CobolLine not provided.");
        
        var allTokens = new List<Token>();

        for (int lineNumber = 0; lineNumber < _lines.Count; lineNumber++)
        {
            CobolLine l = _lines[lineNumber];

            // Column 7 是延續符號
            if (l.Indicator == '-')
            {
                var token = new Token(TokenType.Hyphen, l.Indicator.ToString(), lineNumber, l.Area);
                allTokens.Add(token);
            }

            var tokens = Tokenize(l.Text, l.LineNumber).ToList();
            allTokens.AddRange(tokens);
        }

        return allTokens;
    }

    internal IEnumerable<Token> Tokenize(string line, int lineNumber)
    {
        _pos = 0;

        while (_pos < line.Length)
        {
            char Current = line[_pos];
            
            // Floating comment: *>（最優先）
            if (_pos + 1 < line.Length && line[_pos] == '*' && line[_pos + 1] == '>')
            {
                string comment = line[(_pos + 2)..].Trim();
                yield return new Token(TokenType.Comment, comment, lineNumber);
                break; // ⚠️ 結束 while（本行），不是整個 Tokenize 呼叫
            }
            
            if (IsWordChar(Current))
            {
                yield return ScanWord(line, lineNumber);
                continue;
            }

            // Alphanumeric Literal (String Literal)
            if (Current == '\'')
            {
                yield return ScanAlphanumericLiteral(line, lineNumber, '\'');
                continue;
            }

            // Alphanumeric Literal (String Literal)
            if (Current == '"')
            {
                yield return ScanAlphanumericLiteral(line, lineNumber, '"');
                continue;
            }

            if (Current == '(')
            {
                yield return new Token(TokenType.LParen, line[_pos++].ToString(), lineNumber);
                continue;
            }

            if (Current == ')')
            {
                yield return new Token(TokenType.RParen, line[_pos++].ToString(), lineNumber);
                continue;
            }

            if (Current == '.')
            {
                yield return new Token(TokenType.Dot, line[_pos++].ToString(), lineNumber);
                continue;
            }

            if (char.IsWhiteSpace(Current))
            {
                _pos++;
                continue;
            }

            // ----------------------------
            // Fallback: Unknown char
            // ----------------------------
            yield return new Token(TokenType.Unknown, line[_pos++].ToString(), lineNumber);
        }
    }

    // ----------------------------
    // Scanners
    // ----------------------------

    /// <summary>
    /// 掃描單詞 token，包括： <br/>
    /// - Reserved Word (如 PIC, VALUE, OCCURS) <br/>
    /// - Alphanumeric Literal (識別字/名稱) <br/>
    /// - Numeric Literal <br/>
    /// </summary>
    /// <param name="line"></param>
    /// <param name="lineNumber"></param>
    /// <returns></returns>
    private Token ScanWord(string line, int lineNumber)
    {
        int start = _pos;

        while (_pos < line.Length && IsWordChar(line[_pos]))
            _pos++;

        string word = line[start.._pos];

        return ClassifyWord(word, lineNumber);
    }

    private Token ScanAlphanumericLiteral(string line, int lineNumber, char quoteChar = '\'')
    {
        int start = _pos;
        _pos++; // skip opening quote

        while (_pos < line.Length)
        {
            if (line[_pos] == quoteChar)
            {
                // COBOL 雙引號內的兩個 quote 表示內部 quote
                if (_pos + 1 < line.Length && line[_pos + 1] == quoteChar)
                {
                    _pos += 2; // skip both quotes
                    continue;
                }
                else
                {
                    _pos++; // closing quote
                    break;
                }
            }
            else
            {
                _pos++;
            }
        }

        // 即使沒有閉合也會生成 Token
        string tokenText = line[start.._pos];
        return new Token(TokenType.AlphanumericLiteral, tokenText, lineNumber);
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private static Token ClassifyWord(string word, int lineNumber)
    {
        // NumericLiteral
        if (IsNumeric(word))
            return new Token(TokenType.NumericLiteral, word, lineNumber);

        // Reserved Word or Alphanumeric Literal
        return word switch
        {
            "PICTURE" or
            "PIC"       => new Token(TokenType.Picture  , word, lineNumber),
            "USAGE"     => new Token(TokenType.Usage    , word, lineNumber),
            "DISPLAY"   => new Token(TokenType.Display  , word, lineNumber),
            "COMPUTATIONAL"   or
            "COMP"      => new Token(TokenType.Comp     , word, lineNumber),
            "COMPUTATIONAL-1" or
            "COMP-1"    => new Token(TokenType.Comp1    , word, lineNumber),
            "COMPUTATIONAL-2" or
            "COMP-2"    => new Token(TokenType.Comp2    , word, lineNumber),
            "COMPUTATIONAL-3" or
            "COMP-3"    => new Token(TokenType.Comp3    , word, lineNumber),
            "COMPUTATIONAL-4" or
            "COMP-4"    => new Token(TokenType.Comp4    , word, lineNumber),
            "COMPUTATIONAL-5" or
            "COMP-5"    => new Token(TokenType.Comp5    , word, lineNumber),
            "COMPUTATIONAL-6" or
            "COMP-6"    => new Token(TokenType.Comp6    , word, lineNumber),
            "BINARY"    => new Token(TokenType.Binary   , word, lineNumber),
            "PACKED-DECIMAL" => new Token(TokenType.PackedDecimal, word, lineNumber),
            "VALUE"     => new Token(TokenType.Value    , word, lineNumber),
            "VALUES"    => new Token(TokenType.Values   , word, lineNumber),
            "THROUGH" or
            "THRU"      => new Token(TokenType.Through  , word, lineNumber),
            "SPACE" or
            "SPACES"    => new Token(TokenType.Space    , word, lineNumber),
            "ZERO" or
            "ZEROS" or
            "ZEROES"    => new Token(TokenType.Zero     , word, lineNumber),
            "REDEFINES" => new Token(TokenType.Redefines, word, lineNumber),
            "RENAMES"   => new Token(TokenType.Renames  , word, lineNumber),
            "OCCURS"    => new Token(TokenType.Occurs   , word, lineNumber),
            "TIMES"     => new Token(TokenType.Times    , word, lineNumber),
            "FILLER"    => new Token(TokenType.Filler   , word, lineNumber),

            _ => new Token(TokenType.AlphanumericLiteral, word, lineNumber),
        }; 
    }

    private static bool IsWordChar(char c)
    {
        return char.IsLetterOrDigit(c) || c == '-';
    }

    private static bool IsNumeric(string word)
    {
        if (string.IsNullOrEmpty(word))
            return false;

        foreach (char c in word)
        {
            if (!char.IsDigit(c))
                return false;
        }

        return true;
    }
}