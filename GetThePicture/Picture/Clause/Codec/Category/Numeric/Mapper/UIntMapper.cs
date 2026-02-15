using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Codec.Category.Numeric.Mapper;

public sealed class UIntMapper : MapperBase
{
    protected override void ValidateSign(decimal value, PicMeta pic)
    {
        if (value < 0)
            throw new OverflowException("Unsigned field contains negative value.");
    }
    
    protected override object MapInteger(decimal value, PicMeta pic)
    {
        int digits = pic.IntegerDigits;

        if (digits <= 2)  return (byte)value;
        if (digits <= 4)  return (ushort)value;
        if (digits <= 9)  return (uint)value;
        if (digits <= 18) return (ulong)value;

        return value; // fallback decimal
    }
}
