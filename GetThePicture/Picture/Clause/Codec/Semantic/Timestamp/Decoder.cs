using System.Globalization;
using System.Text;

using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic.Timestamp;

internal static class Decoder
{
    public static DateTime Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        Constraint rule = Rules.GetConstraint(pic.Semantic);
        rule.ValidateOrThrow(pic, pic.Semantic.ToString());

        return pic.Semantic switch
        {
            PicSemantic.Timestamp14 => ParseTimestamp14(buffer),
            _ => throw new NotSupportedException($"Unsupported DateTime format: {pic.Semantic}")
        };
    }

    private static DateTime ParseTimestamp14(ReadOnlySpan<byte> buffer)
    {
        string s = Encoding.ASCII.GetString(buffer);

        // yyyyMMddHHmmss
        if (!DateTime.TryParseExact(
                s, "yyyyMMddHHmmss",
                CultureInfo.InvariantCulture,
                DateTimeStyles.None,
                out var dt))
        {
            throw new FormatException($"Invalid Timestamp14 value: '{s}'");
        }

        return dt;
    }
}
