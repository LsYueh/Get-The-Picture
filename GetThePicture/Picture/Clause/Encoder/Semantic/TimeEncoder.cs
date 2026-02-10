using System.Globalization;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Encoder.Semantic;

internal static class TimeEncoder
{
    public static byte[] Encode(object value, PicMeta pic)
    {
        if (value is not TimeOnly time)
            throw new FormatException($"Invalid value type for TimeOnly encoding: {value.GetType().FullName}");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Time' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        string s = pic.Semantic switch
        {
            PicSemantic.Time6 => time.ToString("HHmmss"   , CultureInfo.InvariantCulture),
            PicSemantic.Time9 => time.ToString("HHmmssfff", CultureInfo.InvariantCulture),
            _ => throw new NotSupportedException($"Unsupported TIME format: {pic.Semantic}")
        };

        return System.Text.Encoding.ASCII.GetBytes(s);
    }
}
