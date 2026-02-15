using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.Computational;

namespace GetThePicture.Picture.Clause.Decoder.Category.Numeric.Mapper;

public abstract class MapperBase : IMapper
{
    public object Map(decimal value, PicMeta pic)
    {
        Validate(value, pic);

        if (pic.DecimalDigits > 0)
            return value;

        return MapInteger(value, pic);
    }

    protected virtual void Validate(decimal value, PicMeta pic)
    {
        ValidateSign(value, pic);
        ValidateIntegerDigits(value, pic);
        ValidateScale(value, pic);
    }

    protected abstract void ValidateSign(decimal value, PicMeta pic);

    protected virtual void ValidateIntegerDigits(decimal value, PicMeta pic)
    {
        if (pic.IntegerDigits <= 0)
            return;

        decimal limit = CbDecimal.Pow10(pic.IntegerDigits);

        if (value >= limit || value <= -limit)
            throw new OverflowException(
                $"Value exceeds declared PIC 9({pic.IntegerDigits}).");
    }

    protected virtual void ValidateScale(decimal value, PicMeta pic)
    {
        if (pic.DecimalDigits > 0)
        {
            decimal scaled = value * CbDecimal.Pow10(pic.DecimalDigits);

            if (scaled != decimal.Truncate(scaled))
                throw new OverflowException(
                    $"Value exceeds declared decimal digits V{pic.DecimalDigits}.");
        }
    }

    protected abstract object MapInteger(decimal value, PicMeta pic);
}