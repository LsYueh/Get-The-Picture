namespace GetThePicture.Copybook.Resolver.Base;

public sealed class ResolverException(string message) : Exception(FormatMessage(message))
{
    private static string FormatMessage(string message)
    {
        return message;
    }
}
