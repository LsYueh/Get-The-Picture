using GetThePicture.Picture.Base.Symbols.Core;

namespace GetThePicture.Picture.Base.Symbols;

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
