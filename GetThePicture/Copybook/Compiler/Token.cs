namespace GetThePicture.Copybook.Compiler;

/// <summary>
/// Copybook 專用 Token 類型
/// </summary>
public enum TokenType
{
    LevelNumber,
    VariableName,

    Character,
    AlphanumericLiteral,
    NumericLiteral,
    Comment,
    Picture, Usage,
    Display, Display1,
    Binary, PackedDecimal, Comp, Comp1, Comp2, Comp3, Comp4, Comp5,
    Date, Time,
    National,
    Filler,
    Number66, Number77, Number88,
    Occurs, Times,
    Redefines, Reference, Renames,
    Value, Values,
    LParen, RParen,
    Dot, EOF,
    
    Identifier, Keyword, Symbol,
    Unknown,
}

public class Token(TokenType type, string value, int lineNumber)
{
    public TokenType Type { get; } = type;
    public string Value { get; } = value;
    public int LineNumber { get; } = lineNumber;

    public override string ToString() => $"{Type}: '{Value}' (Line {LineNumber})";
}
