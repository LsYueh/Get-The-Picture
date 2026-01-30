namespace GetThePicture.Copybook.Compiler.Base;

public sealed class CompileException(string message, Token? token = null) : Exception(FormatMessage(message, token))
{
    public Token? Token { get; } = token;

    private static string FormatMessage(string message, Token? token)
    {
        if (token == null)
            return message;

        return $"{message} (line {token.LineNumber}, token {token.Type}, value '{token.Value}')";
    }
}
