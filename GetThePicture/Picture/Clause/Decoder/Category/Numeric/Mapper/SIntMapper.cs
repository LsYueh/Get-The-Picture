using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Decoder.Category.Numeric.Mapper;

public sealed class SIntMapper : MapperBase
{
    protected override void ValidateSign(decimal value, PicMeta pic)
    {
        // Signed 不需額外檢查
    }
    
    protected override object MapInteger(decimal value, PicMeta pic)
    {
        int digits = pic.IntegerDigits;

        if (digits <= 2)  return (sbyte)value;
        if (digits <= 4)  return (short)value;
        if (digits <= 9)  return (int)value;
        if (digits <= 18) return (long)value;

        return value; // fallback decimal
    }
}
