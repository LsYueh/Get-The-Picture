namespace GetThePicture.Copybook.Compiler;

public class Parser(List<Token> tokens)
{
    private readonly List<Token> tokens = tokens;
    private int position = 0;

    // ----------------------------
    // Pointer
    // ----------------------------

    private Token? Current => position < tokens.Count ? tokens[position] : null;
    
    private Token? Lookahead(int n = 1) => (position + n) < tokens.Count ? tokens[position + n] : null;
    
    // ----------------------------
    // Operations
    // ----------------------------

    private Token Consume() => tokens[position++];

    public Token Expect(TokenType type)
    {
        var current = Current ?? throw new Exception($"Expected {type} but got end of input");
        
        if (current.Type != type)
            throw new Exception($"Expected {type} but got {current.Type}");

        return Consume();
    }

    // ----------------------------
    // Syntactic / Semantic Analysis
    // ----------------------------

    public void Analyze()
    {
        while (Current != null)
        {
            Consume();
        }
    }

    // ----------------------------
    // Grammar
    // ----------------------------
}
