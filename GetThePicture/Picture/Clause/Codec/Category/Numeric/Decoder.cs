using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Base.Computational.Base;
using GetThePicture.Picture.Clause.Base.Options;

using GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

namespace GetThePicture.Picture.Clause.Codec.Category.Numeric;

public static class Decoder
{
    /// <summary>
    /// CP950 → {{ [Overpunch Decode]/[COMP] (object) → Mapper }} → CLR
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="OverflowException"></exception>
    /// <exception cref="NotSupportedException"></exception>
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions? options = null)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException($"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");

        options ??= new CodecOptions();
        
        // Note: COBOL資料記憶體先被S9(n)截位再轉處裡，一般COBOL應該也是這樣的狀況

        // 截位或補字處理
        Span<byte> bytes = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return pic.Usage switch
        {
            PicUsage.Display        => Display_Decode(bytes, pic, options),
            PicUsage.PackedDecimal  =>   COMP3.Decode(bytes, pic),
            PicUsage.Binary         =>   COMP4.Decode(bytes, pic),
            PicUsage.NativeBinary   =>   COMP5.Decode(bytes, pic, options.Binary),
            PicUsage.UPackedDecimal =>   COMP6.Decode(bytes, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };
    }

    private static readonly SIntMapper _SIntMapper = new();
    private static readonly UIntMapper _UIntMapper = new();

    private static object Display_Decode(Span<byte> bytes, PicMeta pic, CodecOptions options)
    {
        Span<byte> chars = Base.Overpunch.OpCodec.Decode(bytes, pic, options, out decimal sign);

        if (chars.Length != pic.DigitCount)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.DigitCount}, actual {chars.Length}.");
        
        bool isNegative = sign < 0;

        decimal value = CbDecimal.Decode(chars, pic.DecimalDigits, isNegative);

        IMapper mapper = pic.Signed ? _SIntMapper : _UIntMapper;

        return (pic.DecimalDigits == 0) ? mapper.Map(value, pic) : value;
    }
}
