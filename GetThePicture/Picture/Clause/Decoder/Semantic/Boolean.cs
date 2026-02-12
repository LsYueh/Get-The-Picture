using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Decoder.Semantic;

internal static class BooleanDecoder
{
    public static bool Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        if (pic.Signed) 
            throw new NotSupportedException($"Unsupported Boolean base type: PIC S9");

        if (pic.Usage != PicUsage.Display)
            throw new NotSupportedException($"'Boolean' does not support usage '{pic.Usage}'. Only DISPLAY is allowed.");

        if (pic.StorageOccupied != 1)
            throw new NotSupportedException($"Boolean must occupy exactly 1 byte in DISPLAY usage. Actual: {pic.StorageOccupied}");

        byte raw = buffer[0];

        // 如果是數字型 PIC 9(1)，'0' = false, '1' = true
        if (pic.BaseClass == PicBaseClass.Numeric)
        {
            if (raw == (byte)'0') return false;
            if (raw == (byte)'1') return true;
            throw new FormatException($"Invalid numeric boolean value: {(char)raw}");
        }

        // 如果是字元型 PIC X(1)，通常 'Y'/'N'
        if (pic.BaseClass == PicBaseClass.Alphanumeric || pic.BaseClass == PicBaseClass.Alphabetic)
        {
            return char.ToUpperInvariant((char)raw) switch
            {
                'Y' => true,
                'N' => false,
                _ => throw new FormatException($"Invalid alphanumeric boolean value: {(char)raw}"),
            };
        }

        throw new NotSupportedException($"Unsupported PIC type for Boolean: {pic.BaseClass}");
    }
}