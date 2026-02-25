using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Forge.Commands.Wrapper.Base;

public static class PicSemanticMap
{
    private static readonly Dictionary<string, PicSemantic> _map =
        new(StringComparer.OrdinalIgnoreCase)
        {
            ["date"]           = PicSemantic.GregorianDate,
            ["minguo-date"]    = PicSemantic.MinguoDate,
            ["time"]           = PicSemantic.Time6,
            ["time-ms"]        = PicSemantic.Time9,
            ["timestamp"]      = PicSemantic.Timestamp14,
            ["bool"]           = PicSemantic.Boolean,
            ["boolean"]        = PicSemantic.Boolean,
        };

    public static bool TryResolve(string? type, out PicSemantic semantic)
    {
        if (string.IsNullOrWhiteSpace(type))
        {
            semantic = PicSemantic.None;
            return false;
        }

        return _map.TryGetValue(type.Trim(), out semantic);
    }
}
