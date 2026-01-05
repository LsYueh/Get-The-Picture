using System.Globalization;
using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolPicEecoder
{
    /// <summary>
    /// CLR value → CP950 → COBOL PICTURE DISPLAY
    /// </summary>
    public static string Encode(object value, PicClause pic, CodecOptions codecOptions)
    {
        ArgumentNullException.ThrowIfNull(value);
        ArgumentNullException.ThrowIfNull(pic);

        byte[] cp950Bytes = EncodeToLogicalBytes(value, pic);

        // TODO: 根據PicClause來處理cp950Bytes...

        throw new NotImplementedException("Encode is not implemented yet.");
    }

    private static byte[] EncodeToLogicalBytes(object value, PicClause pic)
    {
        if (value == null) return [];

        string text = ConvertToLogicalText(value, pic);

        Encoding cp950 = EncodingFactory.CP950;

        return cp950.GetBytes(text);
    }

    /// <summary>
    /// 底層的型別 → 文字 (要跑Test)
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    internal static string ConvertToLogicalText(object value, PicClause pic)
    {
        return value switch
        {
            string s => s,
            char   c => c.ToString(),
            sbyte or byte or short or ushort or int or uint or long or ulong => FormatInvariantText(value),
            float or double or decimal => FormatInvariantText(value),
            DateTime dt => ConvertDateTimeToText(dt, pic),
            _ => throw new NotSupportedException($"Unsupported value type '{value.GetType()}'"),
        };
    }

    private static string FormatInvariantText(object value)
    {
        if (value is IFormattable formattable)
            return formattable.ToString(null, CultureInfo.InvariantCulture);

        return value.ToString()!;
    }

    private static string ConvertDateTimeToText(DateTime dt, PicClause pic)
    {
        // 這裡只是給一個合理預設
        return pic.TotalLength switch
        {
            8  => dt.ToString("yyyyMMdd"),
            // TODO: yyyMMdd
            _  => dt.ToString("yyyyMMdd")
        };
    }
}