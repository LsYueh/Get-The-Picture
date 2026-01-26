namespace GetThePicture.Copybook.Compiler.Base;

public sealed class CompileException(string message, Token token)
    : Exception($"{message} (line {token.LineNumber}, token {token.Type}, value '{token.Value}')")
{
}
