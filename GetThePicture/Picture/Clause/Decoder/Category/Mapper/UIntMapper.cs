using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Decoder.Category.Mapper;

public sealed class UIntMapper : IMapper
{
    public object Map(decimal value, PicMeta pic)
    {
        int digits = pic.IntegerDigits;

        if (digits <= 2)  return (byte)value;
        if (digits <= 4)  return (ushort)value;
        if (digits <= 9)  return (uint)value;
        if (digits <= 18) return (ulong)value;

        // 超過 18 位數，一律用 decimal
        return value;
    }
}
