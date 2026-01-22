namespace GetThePicture.Copybook.Compiler;

public class Lexer
{
    private static readonly HashSet<string> Keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        "VALUES",
        "TO", "DEPENDING", "ON",
        "SYNC", "JUSTIFIED", "RIGHT", "LEFT",
        "SIGN", "LEADING", "TRAILING", "SEPARATE"
    };

    
    public static IEnumerable<Token> Tokenize(string line, int lineNumber)
    {
        int i = 0;

        // Level Number
        if (char.IsDigit(line[i]))
        {
            var start = i;
            while (i < line.Length && char.IsDigit(line[i])) i++;

            yield return new Token(TokenType.LevelNumber,line[start..i],lineNumber);
        }

        while (i < line.Length)
        {
            // Variable Name / Reserved Word / Identifier
            if (IsWordChar(line[i]))
            {
                int start = i;
                while (i < line.Length && IsWordChar(line[i]))
                    i++;

                string word = line[start..i];

                yield return ClassifyWord(word, lineNumber);
                continue;
            }

            // Symbol
            if (IsSymbol(line[i]))
            {
                yield return line[i] switch 
                {
                    '(' => new Token(TokenType.LParen, line[i++].ToString(), lineNumber),
                    ')' => new Token(TokenType.RParen, line[i++].ToString(), lineNumber),
                    '.' => new Token(TokenType.Dot   , line[i++].ToString(), lineNumber),
                     _  => new Token(TokenType.Symbol, line[i++].ToString(), lineNumber),
                };
                
                continue;
            }

            // Alphanumeric Literal
            if (line[i] == '\'')
            {
                int start = i++;
                while (i < line.Length && line[i] != '\'') i++;
                    i++;

                yield return new Token(TokenType.AlphanumericLiteral, line[start..i], lineNumber);
                continue;
            }

            // Whitespace
            if (char.IsWhiteSpace(line[i]))
            {
                i++;
                continue;
            }

            // ----------------------------
            // Fallback: Unknown char
            // ----------------------------
            yield return new Token(TokenType.Unknown, line[i++].ToString(), lineNumber);
        }
    }

    private static Token ClassifyWord(string word, int lineNumber)
    {
        // NumericLiteral
        if (IsNumeric(word))
            return new Token(TokenType.NumericLiteral, word, lineNumber);

        // Keywords
        if (Keywords.Contains(word))
            return new Token(TokenType.Keyword, word.ToUpperInvariant(), lineNumber);

        if (IsBaseDataType(word))
            return new Token(TokenType.Identifier, word, lineNumber);

        return word switch
        {
            "PIC" or "PICTURE" => new Token(TokenType.Picture, word, lineNumber),
            "USAGE"     => new Token(TokenType.Usage    , word, lineNumber),
            "DISPLAY"   => new Token(TokenType.Display  , word, lineNumber),
            "COMP"      => new Token(TokenType.Comp     , word, lineNumber),
            "COMP-1"    => new Token(TokenType.Comp1    , word, lineNumber),
            "COMP-2"    => new Token(TokenType.Comp2    , word, lineNumber),
            "COMP-3"    => new Token(TokenType.Comp3    , word, lineNumber),
            "COMP-4"    => new Token(TokenType.Comp4    , word, lineNumber),
            "COMP-5"    => new Token(TokenType.Comp5    , word, lineNumber),
            "BINARY"    => new Token(TokenType.Binary   , word, lineNumber),
            "PACKED-DECIMAL" => new Token(TokenType.PackedDecimal, word, lineNumber),
            "VALUE"     => new Token(TokenType.Value    , word, lineNumber),
            "REDEFINES" => new Token(TokenType.Redefines, word, lineNumber),
            "RENAMES"   => new Token(TokenType.Renames  , word, lineNumber),
            "OCCURS"    => new Token(TokenType.Occurs   , word, lineNumber),
            "TIMES"     => new Token(TokenType.Times    , word, lineNumber),
            "FILLER"    => new Token(TokenType.Filler   , word, lineNumber),

            _ => new Token(TokenType.VariableName, word, lineNumber),
        }; 
    }

    private static bool IsWordChar(char c)
    {
        return char.IsLetterOrDigit(c) || c == '-';
    }

    /// <summary>
    /// X / 9 / V / S / A
    /// </summary>
    /// <param name="word"></param>
    /// <returns></returns>
    private static bool IsBaseDataType(string word)
    {
        foreach (char c in word)
        {
            if (!"X9VSA".Contains(c))
                return false;
        }

        return true;
    }

    private static bool IsSymbol(char c)
    {
        return @",.()".Contains(c);
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