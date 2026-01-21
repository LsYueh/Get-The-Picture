namespace GetThePicture.Codec.Utils;

internal static class BufferSlice
{
    /// <summary>
    /// 預設 PIC X 的行為，左對齊，右補空白
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="length"></param>
    /// <param name="pad">預設空白</param>
    /// <returns></returns>
    public static byte[] SlicePadEnd(ReadOnlySpan<byte> buffer, int length, byte pad = 0x20)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(length);

        int copyLength = Math.Min(buffer.Length, length);

        // 長度剛好，直接複製回傳
        if (buffer.Length == length)
            return buffer.ToArray();

        byte[] result = new byte[length];

        // 從 buffer 開頭複製到 result
        buffer[..copyLength].CopyTo(result);

        // 如果不足，填充尾端 pad
        if (length > copyLength)
            result.AsSpan(copyLength).Fill(pad);

        return result;
    }


    /// <summary>
    /// 預設 PIC 9/S9 的行為，右對齊，左補'0'
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="offset"></param>
    /// <param name="length"></param>
    /// <param name="pad">預設 '0'</param>
    /// <returns></returns>
    public static byte[] SlicePadStart(ReadOnlySpan<byte> buffer, int length, byte pad = 0x30)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(length);

        int copyLength = Math.Min(buffer.Length, length);

        // 長度剛好，直接複製回傳
        if (buffer.Length == length)
            return buffer.ToArray();

        byte[] result = new byte[length];
        int padLength = length - copyLength;

        // 前面補 pad
        if (padLength > 0)
            result.AsSpan(0, padLength).Fill(pad);

        // 從 buffer 尾端複製到 result 後面
        buffer
            .Slice(buffer.Length - copyLength, copyLength)
            .CopyTo(result.AsSpan(padLength));

        return result;
    }
}
