using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Decoder.Category.Mapper;

public sealed class SIntMapper : IMapper
{
    public object Map(decimal value, PicMeta pic)
    {
        int digits = pic.IntegerDigits;

        if (digits <= 2)  return (sbyte)value;
        if (digits <= 4)  return (short)value;
        if (digits <= 9)  return (int)value;
        if (digits <= 18) return (long)value;

        // 超過 18 位數，一律用 decimal
        return value;
    }
}
