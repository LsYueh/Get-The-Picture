using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic.Boolean;

internal static class Encoder
{
    public static byte[] Encode(object value, PicMeta pic)
    {
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