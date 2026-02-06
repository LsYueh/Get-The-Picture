namespace GetThePicture.Picture.Symbols.Base;

public class Parser(List<Token> tokens)
{
    private readonly List<Token> tokens = tokens;
    private int position = 0;

    // ----------------------------
    // Pointer
    // ----------------------------

    private Token? _previous;

    public Token? Previous => _previous;

    private Token? Current => position < tokens.Count ? tokens[position] : null;

    private Token? Lookahead(int n = 1) => (position + n) < tokens.Count ? tokens[position + n] : null;

    // ----------------------------
    // Operations
    // ----------------------------

    private Token Consume()
    {
        var current = Current ?? throw new InvalidOperationException("Cannot consume EOF.");

        _previous = current;

        return tokens[position++];
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

    public SymbolMeta Analyze()
    { 
        var symbolMeta = new SymbolMeta();
        
        while (Current != null)
        {
            ParseSymbols(symbolMeta);
        }

        return symbolMeta;
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private void ParseSymbols(SymbolMeta symbolMeta)
    {
        
    }
}
