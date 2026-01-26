using System.Text;

using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Cobol.Utils;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.Compiler.Base;

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
    public CbSchema Analyze()
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

        CbSchema root = new();

        while (Current != null)
        {
            ParseDataItem(root);
        }

        return root;
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
    private IDataItem? ParseDataItem(IDataItem? parent = null)
    {
        // 遞迴終止
        if (Current == null) return null;

        // 解析
        IDataItem subordinate = ParseSingleDataItem();

        // 加入 parent 的 Subordinates
        switch (parent)
        {
            case GroupItem g: g.AddSubordinate(subordinate); break;
            case CbSchema  d: d.AddSubordinate(subordinate); break;
        }

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
        
        PicMeta? pic = null;
        string? value = null;
        int? occurs = null;
        var comments = new List<string>();

        CollectComments(comments); // 行首 comment（很少，但合法）

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

        Expect(TokenType.Dot);

        CollectComments(comments);
        
        IDataItem item = BuildDataItem(
            level, name, pic,
            occurs, value,
            isFiller,
            (comments.Count == 0) ? null : string.Join(", ", comments)
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
        int level, string name, PicMeta? pic,
        int? occurs, string? value, bool isFiller = false,
        string? comment = null
    )
    {
        if (pic != null)
        {
            return new ElementaryDataItem(level, name, pic)
            {
                Occurs = occurs,
                Value = value,
                IsFiller = isFiller,
                Comment = comment,
            };
        }

        return new GroupItem(level, name)
        {
            Occurs = occurs,
            Comment = comment,
        };
    }

    private bool IsNextDataItemStart()
    {
        // DataItem 的結束一定是 Dot (.)
        // 但 Dot 後面可能會插入 Floating Comment (TokenType.Comment)
        //
        // 例如：
        //   05 A PIC X.
        //   *> comment
        //   05 B PIC 9.
        //
        // Token 串實際上會是：
        //   Dot -> Comment -> NumericLiteral(05)
        //
        // 因此：
        //   - Comment 不影響結構判斷
        //   - 必須找「前一個非 Comment token」
        //     以及「下一個非 Comment token」來判斷邊界

        return PreviousMeaningfulType() == TokenType.Dot
            && CurrentMeaningfulType()  == TokenType.NumericLiteral;
    }

    /// <summary>
    /// 找前一個有語意的 token（忽略 Comment）
    /// </summary>
    /// <returns></returns>
    private TokenType? PreviousMeaningfulType()
    {
        for (int i = -1; ; i--)
        {
            var t = Lookahead(i);
            if (t == null) return null;
            if (t.Type != TokenType.Comment)
                return t.Type;
        }
    }

    /// <summary>
    /// 找下一個有語意的 token（忽略 Comment）
    /// </summary>
    /// <returns></returns>
    private TokenType? CurrentMeaningfulType()
    {
        for (int i = 0; ; i++)
        {
            var t = Lookahead(i);
            if (t == null) return null;
            if (t.Type != TokenType.Comment)
                return t.Type;
        }
    }

    // ----------------------------
    // Clause Parsers
    // ----------------------------

    private PicMeta ParsePicClause()
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

        PicMeta pic = PicMeta.Parse(picString);
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

        var sb = new StringBuilder();

        while (Current != null && Current.Type != TokenType.Dot)
        {
            switch (Current.Type)
            {
                case TokenType.AlphanumericLiteral:
                    var raw = Consume().Value;
                    var unquoted = UnquoteValuel(raw);
                    sb.Append(unquoted);
                    break;

                case TokenType.NumericLiteral:
                    sb.Append(Consume().Value);
                    break;

                case TokenType.Hyphen:
                    Consume(); // continuation indicator, skip
                    break;

                default:
                    // VALUE 結束（例如遇到 DOT 或下一個 clause）
                    return sb.ToString();
            }
        }


        return sb.ToString();
    }

    // ----------------------------
    // Helpers
    // ----------------------------

    private void CollectComments(List<string> target)
    {
        while (Current != null &&
            Current.Type == TokenType.Comment)
        {
            target.Add(Current.Value.Trim());
            Consume();
        }
    }

    private static string UnquoteValuel(string tokenValue)
    {
        if (string.IsNullOrEmpty(tokenValue))
            return tokenValue;

        char quote = tokenValue[0];

        // 只接受 ' 或 "
        if (quote != '\'' && quote != '"')
            return tokenValue;

        // 必須成對
        if (tokenValue.Length < 2 || tokenValue[^1] != quote)
            return tokenValue; // 不完整 literal，保持原樣

        // 去掉外層 quote
        string inner = tokenValue[1..^1];

        // COBOL escape: '' 或 ""
        string escaped = new([quote, quote]);
        return inner.Replace(escaped, quote.ToString());
    }
}
