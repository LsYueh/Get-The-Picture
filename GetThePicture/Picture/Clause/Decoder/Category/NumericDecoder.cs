using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Computational;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Decoder.Category;

public static class NumericDecoder
{
    /// <summary>
    /// CP950 → [Overpunch Decode]/[COMP] (object) → CLR value
    /// </summary>
    /// <param name="buffer">ASCII/CP950</param>
    /// <param name="pic"></param>
    /// <param name="dataStorageOptions"></param>
    /// <returns></returns>
    /// <exception cref="FormatException"></exception>
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, CodecOptions? options = null)
    {
        options ??= new CodecOptions();

        // Note: COBOL資料記憶體先被S9(n)截位再轉處裡，一般COBOL應該也是這樣的狀況

        // 截位或補字處理
        Span<byte> bytes = Utils.BufferSlice.SlicePadStart(buffer, pic.StorageOccupied);

        return pic.Usage switch
        {
            PicUsage.Display        => Display_Decode(bytes, pic, options),
            PicUsage.PackedDecimal  =>   COMP3.Decode(bytes, pic),
            PicUsage.Binary         =>   COMP4.Decode(bytes, pic, options.Binary),
            PicUsage.NativeBinary   =>   COMP5.Decode(bytes, pic, options.Binary),
            PicUsage.UPackedDecimal =>   COMP6.Decode(bytes, pic),
            _ => throw new NotSupportedException($"Unsupported numeric storage: {pic.Usage}")
        };
    }

    private static object Display_Decode(Span<byte> bytes, PicMeta pic, CodecOptions options)
    {
        var numeric = Base.Overpunch.OpCodec.Decode(bytes, pic, options, out decimal sign);
        return ParseToValue(numeric, sign, pic);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="numeric">數字文</param>
    /// <param name="sign">(+/-)</param>
    /// <param name="pic"></param>
    /// <returns></returns>
    private static object ParseToValue(Span<byte> numeric, decimal sign, PicMeta pic)
    {
        if (pic.DigitCount > 28)
            throw new OverflowException($"PIC {pic} has {pic.IntegerDigits} + {pic.DecimalDigits} = {pic.DigitCount} digit(s), which exceeds the supported maximum (28 digits).");

        if (numeric.Length != pic.DigitCount)
            throw new FormatException($"Numeric length mismatch for PIC. Expected {pic.DigitCount}, actual {numeric.Length}.");
        
        if (pic.DigitCount > 18)
        {
            return ParseBigValue(numeric, pic.IntegerDigits, pic.DecimalDigits, sign);
        }
        else
        {
            return ParseSmallValue(numeric, pic, sign);
        }
    }

    /// <summary>
    /// 解析超過 18 位的數字為 decimal，支援小數點與正負號。
    /// </summary>
    /// <param name="numeric">原始數字字串</param>
    /// <param name="integerDigits">整數位數</param>
    /// <param name="decimalDigits">小數位數</param>
    /// <param name="sign">正負號 (+1 或 -1)</param>
    /// <returns>解析後 decimal</returns>
    private static decimal ParseBigValue(ReadOnlySpan<byte> numeric, int integerDigits, int decimalDigits, decimal sign)
    {
        decimal intPart = 0;
        decimal fracPart = 0;

        int scale = 1; // 用來計算小數位的除法

        // 先解析整數部分
        for (int i = 0; i < integerDigits; i++)
        {
            int digit = numeric[i] - '0';
            if (digit < 0 || digit > 9)
                throw new FormatException($"Invalid digit '{numeric[i]}' in numeric value.");
            intPart = intPart * 10 + digit;
        }

        // 再解析小數部分
        for (int i = integerDigits; i < numeric.Length; i++)
        {
            int digit = numeric[i] - '0';
            if (digit < 0 || digit > 9)
                throw new FormatException($"Invalid digit '{numeric[i]}' in numeric value.");
            fracPart = fracPart * 10 + digit;
            scale *= 10;
        }
            
        // 插入小數點
        decimal value = intPart + (decimalDigits > 0 ? fracPart / scale : 0m);

        // 帶入正負號
        return sign < 0 ? -value : value;
    }

    /// <summary>
    /// 解析 <=18 位數字為 long / decimal，支援小數與正負號。
    /// </summary>
    /// <param name="numeric">數字字串</param>
    /// <param name="pic">PIC 描述</param>
    /// <param name="sign">正負號 (+1 或 -1)</param>
    /// <returns>decimal 或對應 CLR 型別</returns>
    private static object ParseSmallValue(ReadOnlySpan<byte> numeric, PicMeta pic, decimal sign)
    {
        long intPart = 0;
        long fracPart = 0;
        
        int integerDigits = pic.IntegerDigits;
        int decimalDigits = pic.DecimalDigits;

        // 手動 parse
        for (int i = 0; i < numeric.Length; i++)
        {
            int digit = numeric[i] - '0';
            if (digit < 0 || digit > 9)
                throw new FormatException($"Invalid digit '{numeric[i]}' in numeric value.");

            if (i < integerDigits)
                intPart = intPart * 10 + digit;
            else
                fracPart = fracPart * 10 + digit;
        }

        // 帶入正負號
        if (sign < 0)
        {
            intPart = -intPart;
            fracPart = -fracPart;
        }

        // 判斷是否有小數
        if (decimalDigits > 0)
        {
            decimal scale = (decimal)Math.Pow(10, decimalDigits);
            return intPart + fracPart / scale;
        }
        else
        {
            // 沒有小數點，直接返回最小 CLR 型別
            return ConvertToClr(intPart, pic);
        }
    }

    /// <summary>
    /// 整數的 CLR 型別轉換
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static object ConvertToClr(decimal value, PicMeta pic)
    {
        int totalDigits = pic.IntegerDigits;
        bool signed = pic.Signed;

        // 根據 PIC 和 value 決定最佳型別
        
        if (totalDigits <= 2)
        {
            if (!signed) return (byte)value;
            return value >= 0 ? (byte)value : (sbyte)value; // fallback
        }

        if (totalDigits <= 4)
        {
            if (!signed) return (ushort)value;
            return value >= 0 ? (ushort)value : (short)value; // fallback
        }

        if (totalDigits <= 9)
        {
            if (!signed) return (uint)value;
            return value >= 0 ? (uint)value : (int)value; // fallback
        }

        if (totalDigits <= 18)
        {
            if (!signed) return (ulong)value;
            return value >= 0 ? (ulong)value : (long)value; // fallback
        }

        // 超過 18 位數，一律用 decimal
        return value;
    }

}
