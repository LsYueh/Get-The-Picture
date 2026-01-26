namespace GetThePicture.Copybook.SerDes.Record;

internal ref struct RecCursor
{
    private readonly ReadOnlySpan<byte> _buffer;
    private int _offset;

    public RecCursor(ReadOnlySpan<byte> buffer)
    {
        _buffer = buffer;
        _offset = 0;
    }

    public ReadOnlySpan<byte> Read(int length)
    {
        var slice = _buffer.Slice(_offset, length);
        _offset += length;
        return slice;
    }
}
