using GetThePicture.Cobol.Base;

namespace GetThePicture.Copybook.Compiler.Base;

/// <summary>
/// Copybook 專用 Token 類型
/// </summary>
public enum TokenType
{
    Unknown,
    
    Character,
    AlphanumericLiteral,
    NumericLiteral,
    Comment,
    Picture, Usage,
    Display, Display1,
    Binary, PackedDecimal, Comp, Comp1, Comp2, Comp3, Comp4, Comp5, Comp6,
    Date, Time,
    National,
    Filler,
    Occurs, Times,
    Redefines, Reference, Renames,
    Value, Values,
    Space, Zero,
    Through,
    LParen, RParen,
    Dot, Hyphen,
}

public sealed class Token(TokenType type, string value, int lineNumber, Area_t area = Area_t.None)
{
    public TokenType Type { get; } = type;
    public string Value { get; } = value;
    public int LineNumber { get; } = lineNumber;

    public Area_t Area { get; } = area;

    public override string ToString() => $"{Type}: '{Value}' (Line {LineNumber})";
}
