namespace GetThePicture.Picture.Symbols.Base;

/// <summary>
/// PICTURE clause 專用 Token 類型
/// </summary>
public enum TokenType
{
    Unknown,

    // PICTURE clause symbols

    Alphabetic,       // 'A"
    Alphanumeric,     // 'X'
    Numeric,          // 0-9

    Sign,             // 'S'
    ImpliedDecimal,   // 'V' or '.'
    Scaling,          // 'P'

    // Repeat syntax
    
    LParen,           // '('
    RParen,           // ')'
}

public class Token(TokenType type, string value, int pos)
{
    public TokenType Type { get; } = type;
    public string Value { get; } = value;
    /// <summary>
    /// (1-Based)
    /// </summary>
    public int Position { get; } = pos;

    public override string ToString() => $"{Type}: '{Value}' (Position {Position})";
}
