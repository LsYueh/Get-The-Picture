namespace GetThePicture.Copybook.Compiler;

/// <summary>
/// Copybook 專用 Token 類型
/// </summary>
public enum TokenType
{
    LevelNumber,
    Identifier,
    Keyword,
    PictureClause,
    Number,
    StringLiteral,
    Symbol,
    Dot,
    LParen, // (
    RParen, // )
    Unknown,
}

public class Token(TokenType type, string value, int lineNumber)
{
    public TokenType Type { get; } = type;
    public string Value { get; } = value;
    public int LineNumber { get; } = lineNumber;

    public override string ToString() => $"{Type}: '{Value}' (Line {LineNumber})";
}
