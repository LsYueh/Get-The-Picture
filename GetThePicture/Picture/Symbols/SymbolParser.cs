using GetThePicture.Picture.Symbols.Base;

namespace GetThePicture.Picture.Symbols;

public sealed class SymbolParser
{
    private static readonly Lexer _lexer = new();
    private static readonly Parser _parser = new();
    
    public static SymbolMeta Read(string symbols)
    {
        var tokens = _lexer.Tokenize(symbols);
        var meta   = _parser.Analyze(tokens);

        return meta;
    }
}
