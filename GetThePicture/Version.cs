using System.Reflection;

namespace GetThePicture;

public static class Version
{
    private static readonly Assembly _assembly = typeof(Version).Assembly;

    /// <summary>
    /// Semantic version (MinVer)  
    /// e.g. 1.2.3 / 1.2.3-alpha.0+5.gabc123
    /// </summary>
    public static string Informational =>
        _assembly
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()
            ?.InformationalVersion
        ?? "unknown";

    /// <summary>
    /// File version (x.y.z.0)
    /// </summary>
    public static string File =>
        _assembly
            .GetCustomAttribute<AssemblyFileVersionAttribute>()
            ?.Version
        ?? "0.0.0.0";

    /// <summary>
    /// Assembly version (usually fixed)
    /// </summary>
    public static string Assembly =>
        _assembly.GetName().Version?.ToString() ?? "0.0.0.0";
}