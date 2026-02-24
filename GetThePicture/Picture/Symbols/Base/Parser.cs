using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Symbols.Base;

public class Parser()
{
    private List<Token> _tokens = null!;
    private int _pos = 0;

    // ----------------------------
    // Pointer
    // ----------------------------

    private Token? _previous;

    public Token? Previous => _previous;

    private Token? Current => _pos < _tokens.Count ? _tokens[_pos] : null;

    private Token? Lookahead(int n = 1) => (_pos + n) < _tokens.Count ? _tokens[_pos + n] : null;

    // ----------------------------
    // Operations
    // ----------------------------

    private Token Consume()
    {
        var current = Current ?? throw new InvalidOperationException("Cannot consume EOF.");

        _previous = current;

        return _tokens[_pos++];
    }

    private Token Expect(TokenType type)
    {
        var current = Current ?? throw new Exception($"Expected type: '{type}' but got end of input");
        
        if (current.Type != type)
            throw new Exception($"Expected type: '{type}' but got '{current.Type}'");

        return Consume();
    }

    // ----------------------------
    // Analysis
    // ----------------------------

    public SymbolMeta Analyze(List<Token> tokens)
    { 
        _tokens = tokens;
        _pos = 0;
        
        var symbolMeta = new SymbolMeta();
        
        ParseSymbols(symbolMeta);

        return symbolMeta;
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private void ParseSymbols(SymbolMeta symbolMeta)
    {
        bool inDecimal = false;

        while (Current != null)
        {
            var token = Current!;

            switch (token.Type)
            {
                // --------------------
                // Sign
                // --------------------
                case TokenType.Sign:
                    if (symbolMeta.Signed)
                        throw new Exception($"Duplicate Sign at position {token.Position}");
                    
                    symbolMeta.Signed = true;
                    Consume();
                break;

                // --------------------
                // Class
                // --------------------

                case TokenType.Alphabetic:
                case TokenType.Alphanumeric:
                    if (symbolMeta.Signed)
                        throw new Exception($"Sign cannot be combined with {token.Type} at position {token.Position}");

                    ParseRepeat(symbolMeta, inDecimal);
                    break;

                case TokenType.Numeric:
                    ParseRepeat(symbolMeta, inDecimal);
                    break;

                // --------------------
                // ImpliedDecimal (V or .)
                // --------------------
                case TokenType.ImpliedDecimal:
                    if (symbolMeta.BaseClass != PicBaseClass.Numeric)
                        throw new Exception($"Implied decimal can only be used with Numeric class at position {token.Position}");

                    if (inDecimal)
                        throw new Exception($"Multiple implied decimals at position {token.Position}");

                    inDecimal = true;
                    Consume();
                    break;

                // --------------------
                // Scaling (P)
                // --------------------
                // case TokenType.Scaling: Consume(); break; // 先不處理

                // --------------------
                // 不屬於 Symbol 的 token → 拋出例外
                // --------------------
                default:
                    throw new NotSupportedException($"Unsupported token '{token.Value}' of type '{token.Type}' at position {token.Position}");

            }
        }
    }

    /// <summary>
    /// 解析 Numeral / Alpha token，並處理可選的 (Repeat)
    /// 累積到 SymbolMeta 的 IntegerDigits 或 DecimalDigits
    /// </summary>
    private void ParseRepeat(SymbolMeta symbolMeta, bool inDecimal)
    {
        var token = Consume(); // consume current token

        var tokenClass = GetBaseClass(token.Type);

        if (tokenClass == PicBaseClass.Unknown)
            throw new Exception($"Invalid symbol token '{token.Type}' at position {token.Position}");


        // ===== BaseClass lock =====
        if (symbolMeta.BaseClass == PicBaseClass.Unknown)
        {
            symbolMeta.BaseClass = tokenClass;
        }
        else if (symbolMeta.BaseClass != tokenClass)
        {
            // 限制：Numeral / Alphabetic / Alphanumeric 互斥

            throw new Exception($"Cannot mix {tokenClass} with {symbolMeta.BaseClass} at position {token.Position}");
        }

        int repeat = 1;

        // ===== Optional repeat clause =====
        if (Current?.Type == TokenType.LParen)
        {
            Consume(); // consume '('

            // 讀取完整數字
            string text = "";

            while (Current != null && Current.Type == TokenType.Numeric)
            {
                text += Current.Value;
                Consume();
            }

            repeat = int.Parse(text);

            Expect(TokenType.RParen); // consume ')'
        }

        // ===== Semantic accumulation =====
        switch (token.Type)
        {
            case TokenType.Numeric:
                if (inDecimal)
                    symbolMeta.DecimalDigits += repeat;
                else
                    symbolMeta.IntegerDigits += repeat;
                break;

            case TokenType.Alphabetic:
            case TokenType.Alphanumeric:
                symbolMeta.IntegerDigits += repeat;
                break;

            default:
                throw new Exception($"ParseRepeat called on invalid token '{token.Type}' at position {token.Position}");
        }
    }

    private static PicBaseClass GetBaseClass(TokenType type) => type switch
    {
        TokenType.Numeric      => PicBaseClass.Numeric,
        TokenType.Alphabetic   => PicBaseClass.Alphabetic,
        TokenType.Alphanumeric => PicBaseClass.Alphanumeric,
        _ => PicBaseClass.Unknown
    };
}
