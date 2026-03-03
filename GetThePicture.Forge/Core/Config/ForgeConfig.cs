using Microsoft.Extensions.Configuration;

using GetThePicture.Forge.Core.Config.Section;

namespace GetThePicture.Forge.Core.Config;

public class ForgeConfig(IConfiguration config)
{
    private readonly Lazy<IReadOnlyDictionary<string, FieldOverride>> _fields =
        new(() =>
        {
            return config
                .GetSection("fields")
                .Get<Dictionary<string, FieldOverride>>() ?? [];
        });

    public IReadOnlyDictionary<string, FieldOverride> Fields() => _fields.Value;
}
