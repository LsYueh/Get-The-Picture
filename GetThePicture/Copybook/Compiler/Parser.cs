using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
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
        // ParseDataItem (Recursive)
        // │
        // ├─ 1. Parse Single Item
        // │     ├─ Parse Header (Level + Name/FILLER)
        // │     ├─ Parse Clauses (PIC / VALUE / OCCURS)
        // │     └─ Build Item
        // │           ├─ PIC exists → ElementaryDataItem
        // │           └─ No PIC     → GroupItem
        // │
        // └─ 2. Parse Subordinate Items (Group only)
        //       └─ if item is GroupItem
        //            ├─ while nextLevel > currentGroup.Level
        //            └─ ParseDataItem(item) recursively

        return ParseDataItem();
    }

    // ----------------------------
    // COBOL Data Item Parsers/Helpers
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
            while (IsNextDataItemStart())
            {
                int nextLevel = int.Parse(Current.Value);

                if (nextLevel <= group.Level)
                    break;

                ParseDataItem(group);
            }
        }

        return subordinate;
    }

    /// <summary>
    /// 解析單一 COBOL Data Item，
    /// 根據是否出現 PIC 子句判定為 ElementaryDataItem 或 GroupItem，
    /// 並處理其附屬的子句（如 OCCURS、VALUE）。
    /// </summary>
    /// <returns></returns>
    /// <exception cref="CompileException"></exception>
    private IDataItem ParseSingleDataItem()
    {
        // [ Tokens ]
        //     │
        //     ▼
        // [ DataItem Header ]
        //     │
        //     ▼
        // [ Clauses Collection ]
        //  (PIC / VALUE / OCCURS)
        //     │
        //     ▼
        // [ BuildDataItem ]
        //     ├─ PIC exists  ──► ElementaryDataItem
        //     └─ no PIC      ──► GroupItem
        
        PicClause? pic = null;
        string? value = null;
        int? occurs = null;

        var (level, name, isFiller) = ParseDataItemHeader();

        while (Current != null && Current.Type != TokenType.Dot)
        {
            switch (Current?.Type)
            {
                case TokenType.Picture:
                    pic = ParsePicClause();
                    break;

                case TokenType.Occurs:
                    occurs = ParseOccursClause();
                    break;

                case TokenType.Value:
                    value = ParseValueClause();
                    break;

                default:
                    throw new CompileException($"Invalid or unsupported clause after data item '{name}'.", Current ?? Previous);
            }
        }

        Expect(TokenType.Dot); // 結束
        
        IDataItem item = BuildDataItem(
            level, name, pic,
            occurs, value,
            isFiller
        );

        return item;
    }

    private (int Level, string Name, bool IsFiller) ParseDataItemHeader()
    {
        int level = int.Parse(Expect(TokenType.NumericLiteral).Value);

        if (Current?.Type == TokenType.Filler)
        {
            Consume(); // FILLER
            return (level, "FILLER", true);
        }

        string name = Expect(TokenType.AlphanumericLiteral).Value;
        return (level, name, false);
    }

    private static IDataItem BuildDataItem(
        int level, string name, PicClause? pic,
        int? occurs, string? value, bool isFiller = false 
    )
    {
        if (pic != null)
        {
            return new ElementaryDataItem(level, name, pic)
            {
                Occurs = occurs,
                Value = value,
                IsFiller = isFiller,
            };
        }

        return new GroupItem(level, name)
        {
            Occurs = occurs,
        };
    }

    private bool IsNextDataItemStart()
    {
        var previousType = Previous?.Type;
        var currentType  = Current?.Type;
        
        // Note: ParseSingleDataItem() 已經 Expect(TokenType.Dot)，所以要看 Previous

        return Current != null
            && previousType == TokenType.Dot
            && currentType == TokenType.NumericLiteral;
    }

    // ----------------------------
    // Clause Parsers
    // ----------------------------

    private PicClause ParsePicClause()
    {
        Consume(); // PIC

        PicUsage usage = PicUsage.Display;

        string picString = "";
        while (Current != null &&
            Current.Type != TokenType.Dot &&
            Current.Type != TokenType.Occurs &&
            Current.Type != TokenType.Value
        )
        {
            switch (Current.Type)
            {   
                case TokenType.AlphanumericLiteral: // X, A, S, V, etc.
                case TokenType.NumericLiteral: // 9
                    picString += Consume().Value;
                    break;

                case TokenType.LParen: // 括號數量
                    picString += Expect(TokenType.LParen).Value;
                    picString += Expect(TokenType.NumericLiteral).Value;
                    picString += Expect(TokenType.RParen).Value;
                    break;

                case TokenType.Usage:
                    Consume(); // Usage
                    break;

                case TokenType.Binary:
                case TokenType.Comp:
                    Consume();
                    usage = PicUsage.Binary;
                    break;
                case TokenType.Comp5:
                    Consume();
                    usage = PicUsage.NativeBinary;
                    break;
                case TokenType.PackedDecimal:
                case TokenType.Comp3:
                    Consume();
                    usage = PicUsage.PackedDecimal;
                    break;
                case TokenType.Comp1:
                case TokenType.Comp2:
                case TokenType.Comp4:
                    throw new Exception("COMP-1/2/4 not supported yet.");

                default:
                    throw new CompileException(
                        $"Invalid token in PIC clause: {Current.Value}",
                        Current
                    );
            }
        }

        PicClause pic = Pic.Parse(picString);
        pic.Usage = usage;

        return pic;
    }

    private int ParseOccursClause()
    {
        Consume(); // OCCURS

        if (Current == null)
            throw new CompileException("OCCURS clause requires a literal.", Previous);

        int occurs = int.Parse(Expect(TokenType.NumericLiteral).Value);

        if (Current.Type == TokenType.Times) Consume(); // TIMES

        return occurs;
    }

    private string ParseValueClause()
    {
        Consume(); // VALUE

        if (Current == null)
            throw new CompileException("VALUE clause requires a literal.", Previous);

        return Current.Type switch
        {
            TokenType.AlphanumericLiteral => Consume().Value,
            TokenType.NumericLiteral      => Consume().Value,
            _ => throw new CompileException($"Invalid VALUE literal: {Current.Value}", Current),
        };
    }

}
