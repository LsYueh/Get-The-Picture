using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.Compiler;

public sealed record DataItemHeader(
    int Level,
    string Name
);

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
    // Syntactic / Semantic Analysis
    // ----------------------------

    /// <summary>
    /// Syntactic / Semantic Analysis
    /// </summary>
    public IDataItem? Analyze()
    {
        return ParseDataItem();
    }

    // ----------------------------
    // Grammar
    // ----------------------------

    /// <summary>
    /// Recursive Descent Parser (Top-down Parser).
    /// </summary>
    /// <param name="parent"></param>
    /// <returns></returns>
    /// <exception cref="CompileException"></exception>
    private IDataItem? ParseDataItem(GroupItem? parent = null)
    {
        // 遞迴終止
        if (Current == null) return null;

        // 解析
        IDataItem subordinate = ParseSingleDataItem();

        parent?.AddSubordinate(subordinate);

        // 解析子項目（只有 GroupItem 才能有 subordinate）
        if (subordinate is GroupItem group)
        {
            while (Current != null && Current.Type == TokenType.LevelNumber)
            {
                int nextLevel = int.Parse(Current.Value);

                if (nextLevel <= group.Level)
                    break;

                ParseDataItem(group);
            }
        }

        return subordinate;
    }

    private (int Level, string Name) ParseDataItemHeader()
    {
        var level = int.Parse(Expect(TokenType.LevelNumber).Value);
        var name  = Expect(TokenType.AlphanumericLiteral).Value;

        return (level, name);
    }

    private IDataItem ParseSingleDataItem()
    {
        var (level, name) = ParseDataItemHeader();

        IDataItem item;

        switch (Current?.Type)
        {
            case TokenType.Dot:
                item = new GroupItem(level, name);
                break;

            case TokenType.Occurs:
                Consume(); // OCCURS
                int occurs = int.Parse(Expect(TokenType.NumericLiteral).Value);
                item = new GroupItem(level, name, occurs);
                break;

            case TokenType.Picture:
                Consume(); // PIC

                string picString = "";
                picString += Expect(TokenType.Identifier).Value;
                picString += Expect(TokenType.LParen).Value;
                picString += Expect(TokenType.NumericLiteral).Value;
                picString += Expect(TokenType.RParen).Value;

                var pic = Pic.Parse(picString);
                item = new ElementaryDataItem(level, name, pic);
                break;

            default:
                throw new CompileException($"Invalid or unsupported clause after data item '{name}'.", Current ?? Previous);
        }

        Expect(TokenType.Dot);

        return item;
    }

}
