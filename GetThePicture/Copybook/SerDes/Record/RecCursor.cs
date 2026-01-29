namespace GetThePicture.Copybook.SerDes.Record;

internal ref struct RecCursor
{
    private readonly ReadOnlySpan<byte> _buffer;
    private int _offset;

    public readonly int Size => _buffer.Length;

    public RecCursor(ReadOnlySpan<byte> buffer)
    {
        _buffer = buffer;
        _offset = 0;
    }

    public ReadOnlySpan<byte> Read(int length)
    {
        if (_offset + length > _buffer.Length)
            throw new InvalidOperationException($"Read overflow: offset={_offset}, length={length}, size={_buffer.Length}");
        
        var slice = _buffer.Slice(_offset, length);
        _offset += length;
        return slice;
    }
}
