namespace GetThePicture.Codec.Utils;

internal static class BufferSlice
{
    /// <summary>
    /// 
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="offset"></param>
    /// <param name="length"></param>
    /// <param name="pad">預設空白</param>
    /// <returns></returns>
    public static ReadOnlySpan<byte> SlicePadEnd(
        byte[] buffer,
        int offset, int length,
        byte pad = 0x20)
    {
        ArgumentNullException.ThrowIfNull(buffer);
        ArgumentOutOfRangeException.ThrowIfNegative(offset);
        ArgumentOutOfRangeException.ThrowIfNegative(length);

        // 完整落在 buffer 範圍內 → 零拷貝
        if ((uint)offset <= (uint)buffer.Length &&
            (uint)length <= (uint)(buffer.Length - offset))
        {
            return buffer.AsSpan(offset, length);
        }

        // offset 已超出 → 全 padding
        if (offset >= buffer.Length)
        {
            byte[] padded = new byte[length];
            padded.AsSpan().Fill(pad);
            return padded;
        }

        // 部分不足 → copy + pad
        int available = buffer.Length - offset;

        byte[] result = new byte[length];
        buffer.AsSpan(offset, available).CopyTo(result);
        result.AsSpan(available).Fill(pad);

        return result;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="offset"></param>
    /// <param name="length"></param>
    /// <param name="pad"> 預設 '0'</param>
    /// <returns></returns>
    public static ReadOnlySpan<byte> SlicePadStart(
        byte[] buffer,
        int offset, int length,
        byte pad = 0x30)
    {
        ArgumentNullException.ThrowIfNull(buffer);
        ArgumentOutOfRangeException.ThrowIfNegative(offset);
        ArgumentOutOfRangeException.ThrowIfNegative(length);

        // 完整命中
        if ((uint)offset <= (uint)buffer.Length &&
            (uint)length <= (uint)(buffer.Length - offset))
        {
            return buffer.AsSpan(offset, length);
        }

        // offset 超出 → 全 pad
        if (offset >= buffer.Length)
        {
            byte[] padded = new byte[length];
            padded.AsSpan().Fill(pad);
            return padded;
        }

        // 部分不足 → 前補
        int available = buffer.Length - offset;

        byte[] result = new byte[length];
        buffer.AsSpan(offset, available).CopyTo(result.AsSpan(length - available));
        result.AsSpan(0, length - available).Fill(pad);

        return result;
    }
}
