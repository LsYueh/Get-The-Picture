using System.Globalization;

using GetThePicture.Picture.Base.Clause.Items;
using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Picture.Codec.Semantic.Time;

internal static class Encoder
{
    public static byte[] Encode(object value, PicMeta pic)
    {
        if (value is not TimeOnly time)
            throw new FormatException($"Invalid value type for TimeOnly encoding: {value.GetType().FullName}");

        string s = pic.Semantic switch
        {
            PicSemantic.Time6 => time.ToString("HHmmss"   , CultureInfo.InvariantCulture),
            PicSemantic.Time9 => time.ToString("HHmmssfff", CultureInfo.InvariantCulture),
            _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
        };

        return System.Text.Encoding.ASCII.GetBytes(s);
    }
}
