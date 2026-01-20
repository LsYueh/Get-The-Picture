using System.Text;

using GetThePicture.Cobol.Display;
using GetThePicture.Cobol.Picture;
using GetThePicture.Cobol.Picture.ComputationalBase;
using GetThePicture.Cobol.Picture.OverpunchBase;
using GetThePicture.Cobol.Picture.TypeBase;
using GetThePicture.Codec.Options;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Codec.Encoder.Category;

internal static class NumericEncoder
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

        byte[] buffer = pic.Usage switch
        {
            PicUsage.Display       => Display_Encode(displayValue, pic, options),
            PicUsage.Binary        =>    COMP.Encode(displayValue, pic),
            PicUsage.PackedDecimal =>   COMP3.Encode(displayValue, pic),
            PicUsage.NativeBinary  =>   COMP5.Encode(displayValue, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };

        // 截位或補字處理
        ReadOnlySpan<byte> fieldBytes = BufferSlice.SlicePadStart(buffer, pic.DigitCount);

        // 輸出
        Encoding cp950 = EncodingFactory.CP950;
        return cp950.GetString(fieldBytes);
    }

    private static byte[] Display_Encode(DisplayValue displayValue, PicClause pic, CodecOptions options)
    {
        string numeric = displayValue switch
        {
            { Kind: DisplayValueKind.Number, Number: { } n } => n.Digits,
            _ => throw new NotSupportedException($"Unsupported Display Value Kind '{displayValue.Kind}' for Numeric Text"),
        };

        decimal sign = displayValue.Sign;

        byte[] buffer = Overpunch.Encode(sign, numeric, pic, options);

        return buffer;
    }
}