using System.Text.Json.Serialization;

namespace GetThePicture.Forge.Core.Config.Section;

public class FieldOverride
{
    [JsonPropertyName("type")]
    public string? Type { get; set; }

    [JsonPropertyName("comment")]
    public string? Comment { get; set; }
}