using System.Text;

using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Base;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

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
    public CbLayout Analyze()
    {
        // ParseDataItem (Recursive)
        // │
        // ├─ 1. Parse Single Item
        // │     ├─ Parse Header (Level + Name/FILLER)
        // │     ├─ Parse Clauses (PIC / VALUE / OCCURS)
        // │     └─ Build Item
        // │           └─ ElementaryDataItem / GroupItem / etc...
        // │
        // └─ 2. Parse Subordinate Items (Group only)
        //       └─ if item is GroupItem
        //            ├─ while nextLevel > currentGroup.Level
        //            └─ ParseDataItem(item) recursively

        CbLayout root = new();

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
        IDataItem item = ParseSingleDataItem();

        // 加入 parent 的 Subordinates / 88 處理
        switch (parent)
        {
            case ElementaryDataItem e:
            {
                if (item is Condition88Item _88Item)
                {
                    e.AddCondition(_88Item); break;
                }
                    
                throw new CompileException("Elementary data item cannot have subordinates.", Current ?? Previous);
            }
            case RedefinesItem r: r.AddSubordinate(item); break;
            case GroupItem g: g.AddSubordinate(item); break;
        }

        // 過濾可遞迴的子項
        IDataItem? parentItem = item switch
        {
            RedefinesItem r => r,
            GroupItem g => g,
            ElementaryDataItem e => e,
            _ => null
        };

        if (parentItem != null)
        {
            int parentLevel = parentItem.Level;
            while (IsNextDataItemStart())
            {
                int nextLevel = int.Parse(Current.Value);
                
                // Level 層級結束
                if (nextLevel <= parentLevel) break;

                // Level 66 是 record-level 的語意節點，除了 Root 外不屬於任何 GroupItem 或 ElementaryDataItem 
                if (nextLevel == 66) break;
                
                ParseDataItem(parentItem);
            }
        }

        return item;
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
        // [ (Build DataItem) ]
        //     ├─ Lv 1 ~ 49 ──► ElementaryDataItem / GroupItem
        //     ├─ Lv 66 ──► Renames66Item
        //     ├─ Lv 77 ──► [Unsupported]
        //     └─ Lv 88 ──► Condition88Item
        
        // Lv 1 ~ 49

        PicMeta? pic = null;
        string? value = null;
        int? occurs = null;
        var comments = new List<string>();

        // REDEFINES
        string? targetName = null;

        // Level 66

        string  lv66From    = string.Empty;
        string? lv66Through = null;

        // Level 88

        IEnumerable<object>? lv88Values  = null;
        object?              lv88Through = null;

        // ----------------------------

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

                case TokenType.Redefines:
                {
                    targetName = ParseRedefinesClause();
                    break;
                }

                case TokenType.Renames:
                {
                    if (level != 66)
                        throw new CompileException($"'RENAMES' clause is only valid for level 66 items.", Current ?? Previous);
                    
                    (lv66From, lv66Through) = ParseLv66RenamesClause();
                    
                    break;
                }

                case TokenType.Value:
                    if (level == 88) {
                        (lv88Values, lv88Through) = ParseLv88ValuesClause();
                    }
                    else
                    {
                        value = ParseValueClause();
                    }
                    break;

                case TokenType.Values:
                {
                    if (level != 88)
                        throw new CompileException($"'VALUES' clause is only valid for level 88 items.", Current ?? Previous);
                
                    (lv88Values, lv88Through) = ParseLv88ValuesClause();
                    break;
                }

                default:
                    throw new CompileException($"Invalid or unsupported clause after data item '{name}'.", Current ?? Previous);
            }
        }

        Expect(TokenType.Dot);

        CollectComments(comments);

        string? comment = (comments.Count == 0) ? null : string.Join(", ", comments);

        // Build Normal DataItem
        IDataItem CreateLevel1To49()
        {
            // REDEFINES 優先處理
            if (targetName is not null)
                return new RedefinesItem(level, name, targetName, comment);
            
            if (pic is null)
                return new GroupItem(level, name, occurs, comment);

            var item = new ElementaryDataItem(level, name, pic, occurs, value, isFiller, comment);
            
            return item;
        }

        // Build Full DataItem
        IDataItem item = level switch
        {
            >= 1 and <= 49 => CreateLevel1To49(),
            66 => new Renames66Item(name, lv66From, lv66Through, comment),
            88 => new Condition88Item(name, lv88Values, lv88Through),
            _ => throw new CompileException($"Unsupported level {level} for data item '{name}'", Current ?? Previous),
        };

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

                // COBOL COMPUTATIONAL

                case TokenType.Comp3:
                    Consume();
                    usage = PicUsage.PackedDecimal;
                    break;
                case TokenType.Binary:
                case TokenType.Comp:
                case TokenType.Comp4:
                    Consume();
                    usage = PicUsage.Binary;
                    break;
                case TokenType.Comp5:
                    Consume();
                    usage = PicUsage.NativeBinary;
                    break;
                case TokenType.PackedDecimal:
                case TokenType.Comp6:
                    Consume();
                    usage = PicUsage.UPackedDecimal;
                    break;
                case TokenType.Comp1:
                case TokenType.Comp2:
                    throw new Exception("COMP-1/2 not supported yet.");

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

                case TokenType.Space:
                    Consume(); break;

                case TokenType.NumericLiteral:
                    sb.Append(Consume().Value);
                    break;

                case TokenType.Zero:
                    sb.Append('0');
                    Consume(); break;

                case TokenType.Hyphen:
                    Consume(); // continuation indicator, skip
                    break;

                default:
                    // VALUE 結束（例如遇到 DOT 或下一個 clause）
                    return sb.ToString();
            }
        }

        return sb.ToString(); // TODO: 要根據TokenType輸出成string或decimal...
    }

    private string ParseRedefinesClause()
    {
        Consume(); // REDEFINES

        string targetName = Expect(TokenType.AlphanumericLiteral).Value;

        return targetName;
    }

    private (string From, string? Thru) ParseLv66RenamesClause()
    {
        Consume(); // RENAMES

        string  from = Expect(TokenType.AlphanumericLiteral).Value;
        string? thru = null;

        if (Current?.Type == TokenType.Through)
        {
            Consume(); // THRU / THROUGH
            thru = Expect(TokenType.AlphanumericLiteral).Value;
        }

        return (from, thru);
    }

    private (IEnumerable<object>? Values, object? Through) ParseLv88ValuesClause()
    {
        // VALUE 或 VALUES
        switch (Current?.Type)
        {
            case TokenType.Value:
                Consume(); // VALUE
                break;
            case TokenType.Values:
                Consume(); // VALUES
                break;
            default:
                throw new CompileException($"'VALUE(S)' clause is required for level 88 items.", Current ?? Previous);
        }

        var values = new List<object>();
        object? through = null;

        // 解析第一個值
        values.Add(ParseLv88SingleValue());

        // 後續可能是：
        // VALUE A B C
        // VALUE 1 THROUGH 9
        // VALUE 'A' THROUGH 'Z'

        while (Current != null && Current.Type != TokenType.Dot)
        {
            if (Current.Type == TokenType.Through)
            {
                Consume(); // THROUGH
                through = ParseLv88SingleValue();
                break; // THROUGH 結束語意範圍，不再接續值
            }

            // 多值情況
            values.Add(ParseLv88SingleValue());
        }
            
        return (values.Count == 0 ? null : values, through);
    }

    private object ParseLv88SingleValue()
    {
        if (Current == null)
            throw new CompileException("Unexpected end of input in level 88 VALUE clause.", Previous);

        switch (Current.Type)
        {
            case TokenType.AlphanumericLiteral:
                var raw = Consume().Value;
                var unquoted = UnquoteValuel(raw);
                return unquoted;

            case TokenType.NumericLiteral:
                var n = Consume().Value;
                return int.TryParse(n, out var i) ? i : decimal.Parse(n);

            default:
                throw new CompileException($"Invalid VALUE token for level 88: {Current.Type}", Current);
        }
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
