using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Picture.Clause.Codec;

public static class Initializer
{
    public static byte[] Initialize(PicMeta pic, CodecOptions options)
    {
        ArgumentNullException.ThrowIfNull(pic);

        byte[] normalized =  pic.BaseClass switch
        {
            PicBaseClass.Numeric => Encoder.EncodeBaseType(0, pic, options),
            PicBaseClass.Alphanumeric or 
            PicBaseClass.Alphabetic => Encoder.EncodeBaseType(string.Empty, pic, options),
            _ => throw new NotSupportedException($"Unsupported PIC Data Type [Encode] : {pic.BaseClass}"),
        };

        return normalized;
    }
}
