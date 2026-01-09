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

        byte[] result = new byte[length];
        int copyLength = Math.Min(buffer.Length, length);

        // 從 buffer 開頭複製到 result
        Array.Copy(buffer.ToArray(), 0, result, 0, copyLength);

        // 如果不足，填充尾端 pad
        if (length > copyLength)
            Array.Fill(result, pad, copyLength, length - copyLength);

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

        byte[] result = new byte[length];
        int copyLength = Math.Min(buffer.Length, length);
        int padLength = length - copyLength;

        // 填充前面的 pad
        if (padLength > 0)
            Array.Fill(result, pad, 0, padLength);

        // 從 buffer 尾端複製到 result 後面
        Array.Copy(buffer.ToArray(), buffer.Length - copyLength, result, padLength, copyLength);

        return result;
    }
}
