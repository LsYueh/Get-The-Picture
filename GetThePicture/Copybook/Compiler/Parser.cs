using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Compiler.Ir;

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

    private Token Expect(TokenType type)
    {
        var current = Current ?? throw new Exception($"Expected type: '{type}' but got end of input");
        
        if (current.Type != type)
            throw new Exception($"Expected type: '{type}' but got '{current.Type}'");

        return Consume();
    }

    // ----------------------------
    // Syntactic / Semantic Analysis
    // ----------------------------

    /// <summary>
    /// Syntactic / Semantic Analysis
    /// </summary>
    public IDataItem? Analyze()
    {
        IDataItem? root = null;
        
        while (Current != null)
        {
            var level = int.Parse(Expect(TokenType.LevelNumber).Value);
            var name  = Expect(TokenType.AlphanumericLiteral).Value;

            switch (Current.Type)
            {
                case TokenType.Dot:
                {
                    root = new GroupItem(level, name);
                    break;
                }
                case TokenType.Occurs:
                {
                    Consume(); // OCCURS
                    int occurs = int.Parse(Expect(TokenType.NumericLiteral).Value);

                    root = new GroupItem(level, name, occurs);
                    break;
                }
                case TokenType.Picture:
                {
                    string picString = "";

                    // TODO: Pic Grammar Parser

                    Consume(); // PIC
                    picString += Expect(TokenType.Identifier).Value;
                    picString += Expect(TokenType.LParen).Value;
                    picString += Expect(TokenType.NumericLiteral).Value;
                    picString += Expect(TokenType.RParen).Value;

                    var pic = Pic.Parse(picString);
                    root = new ElementaryDataItem(level, name, pic);
                    break;
                }
                default:
                    throw new CompileException(
                        $"Invalid or unsupported clause after data item '{name}'. " +
                        $"Expected OCCURS or PIC.",
                        Current
                    );
            }

            Expect(TokenType.Dot);
        }

        return root;
    }

    // ----------------------------
    // Grammar
    // ----------------------------


}
