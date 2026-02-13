using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Decoder.Category;

public static class Clr
{
    /// <summary>
    /// 整數的 CLR 型別轉換
    /// </summary>
    /// <param name="value"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static object AsInteger(decimal value, PicMeta pic)
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