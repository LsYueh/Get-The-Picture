using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Encoder.Semantic;

internal static class BooleanEncoder
{
    public static byte[] Encode(object value, PicMeta pic)
    {
        if (pic.Signed)
            throw new NotSupportedException($"Unsupported Boolean base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Boolean' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        if (pic.StorageOccupied != 1)
            throw new NotSupportedException($"Boolean must occupy exactly 1 byte in DISPLAY usage. Actual: {pic.StorageOccupied}");

        if (value is not bool b)
            throw new ArgumentException($"Value must be of type bool, got {value?.GetType().Name}");
        var encoded = pic.BaseClass switch
        {
            // 0/1
            PicBaseClass.Numeric => b ? (byte)'1' : (byte)'0',
            // Y/N
            PicBaseClass.Alphanumeric or PicBaseClass.Alphabetic => b ? (byte)'Y' : (byte)'N',
            
            _ => throw new NotSupportedException($"Unsupported PIC type for Boolean: {pic.BaseClass}"),
        };
        
        return [encoded];
    }
}