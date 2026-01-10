using System.Text;

using GetThePicture.Cobol;
using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder;

internal static class CobolNumericEncoder
{
    /// <summary>
    /// Display Value → Overpunch Encode → COBOL PICTURE DISPLAY
    /// </summary>
    /// <param name="displayValue"></param>
    /// <param name="pic"></param>
    /// <param name="options"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static string Encode(DisplayValue displayValue, PicClause pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        // Note: 要先變成DISPLAY的VALUE，再模擬DISPLAY時被S9(n)截位的輸出結果

        string numeric = displayValue switch
        {
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Numeric Text"),
        };

        decimal sign = displayValue.Sign;

        // 轉換
        byte[] buffer = Overpunch.Encode(sign, numeric, pic, options);

        // 截位或補字處理
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(buffer, pic.TotalLength);

        // 輸出
        Encoding cp950 = EncodingFactory.CP950;
        return cp950.GetString(fieldBytes);
    }
}