namespace GetThePicture.Copybook.SerDes.Field;

public readonly ref struct CbFieldAccessor
{
    // Union Buffer
    public readonly ReadOnlySpan<byte> Raw;

    public CbFieldAccessor(ReadOnlySpan<byte> raw)
    {
        Raw = raw;
    }

    /// <summary>從 Raw 讀取指定區段</summary>
    public ReadOnlySpan<byte> Read(int offset, int length)
    {
        if (offset + length > Raw.Length)
            throw new InvalidOperationException($"Read overflow: offset={offset}, length={length}, size={Raw.Length}");

        return Raw.Slice(offset, length);
    }
}
