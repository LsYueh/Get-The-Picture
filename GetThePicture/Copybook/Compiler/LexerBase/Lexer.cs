namespace GetThePicture.Copybook.Compiler.LexerBase;

public class Lexer
{
    private static readonly HashSet<string> Keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        "PIC", "PICTURE",

        "VALUE", "VALUES",

        "USAGE", "DISPLAY",
        "COMP", "COMP-1", "COMP-2", "COMP-3", "COMP-4", "COMP-5",
        "BINARY", "PACKED-DECIMAL",

        "OCCURS", "TIMES",
        "TO", "DEPENDING",
        "ON",

        "REDEFINES", "RENAMES",

        "FILLER",

        "SYNC", "JUSTIFIED", "RIGHT", "LEFT",

        "SIGN", "LEADING", "TRAILING", "SEPARATE"
    };

    
    public static IEnumerable<Token> Tokenize(string line, int lineNumber)
    {
        int i = 0;

        // Level number 必定在開頭
        if (char.IsDigit(line[i]))
        {
            var start = i;
            while (i < line.Length && char.IsDigit(line[i])) i++;

            yield return new Token(
                TokenType.LevelNumber,
                line[start..i],
                lineNumber
            );
        }

        while (i < line.Length)
        {
            // Word (IDENTIFIER / KEYWORD / NUMBER)
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
                yield return new Token(TokenType.Symbol, line[i++].ToString(), lineNumber);
                continue;
            }

            // String literal
            if (line[i] == '\'')
            {
                int start = i++;
                while (i < line.Length && line[i] != '\'') i++;
                    i++;

                yield return new Token(TokenType.StringLiteral, line[start..i], lineNumber);
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

    private static bool IsWordChar(char c)
    {
        return char.IsLetterOrDigit(c) || c == '-';
    }

    private static bool IsSymbol(char c)
    {
        return ".()".Contains(c);
    }

    private static bool IsNumber(string word)
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

    private static Token ClassifyWord(string word, int lineNumber)
    {
        // Keywords
        if (Keywords.Contains(word))
            return new Token(TokenType.Keyword, word.ToUpperInvariant(), lineNumber);
        
        // Number
        if (IsNumber(word))
            return new Token(TokenType.Number, word, lineNumber);

        // Identifier
        return new Token(TokenType.Identifier, word, lineNumber);
    }

}