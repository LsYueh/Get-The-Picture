using System.Text;

using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec.Utils;

[TestClass]
public class BufferSliceTest
{
    private static byte[] Bytes(string s) => Encoding.ASCII.GetBytes(s);

    private static string Text(ReadOnlySpan<byte> span) => Encoding.ASCII.GetString(span);

    [TestMethod]
    public void ExactOrOverLength_ShouldSliceFromEnd()
    {
        // buffer = "123456"
        var buffer = Bytes("123456");

        var result = BufferSlice.SlicePadStart(buffer, length: 4);

        Assert.AreEqual("3456", Text(result));
    }

    [TestMethod]
    public void OffsetInsideBuffer_ShouldSliceFromEnd()
    {
        // buffer = "123456", offset = 2 → "3456"
        var buffer = Bytes("123456");

        var result = BufferSlice.SlicePadStart(buffer, offset: 2, length: 4);

        Assert.AreEqual("3456", Text(result));
    }

    [TestMethod]
    public void InsufficientLength_ShouldPadStart()
    {
        // buffer = "123"
        var buffer = Bytes("123");

        var result = BufferSlice.SlicePadStart(buffer, length: 5);

        Assert.AreEqual("00123", Text(result));
    }

    [TestMethod]
    public void OffsetAtEnd_ShouldReturnAllPad()
    {
        // offset == buffer.Length
        var buffer = Bytes("123");

        var result = BufferSlice.SlicePadStart(buffer, offset: 3, length: 4);

        Assert.AreEqual("0000", Text(result));
    }

    [TestMethod]
    public void ExactLength_ShouldReturnOriginal()
    {
        var buffer = Bytes("123456");

        var result = BufferSlice.SlicePadStart(buffer, length: 6);

        Assert.AreEqual("123456", Text(result));
    }

    [TestMethod]
    public void LengthZero_ShouldReturnEmpty()
    {
        var buffer = Bytes("123456");

        var result = BufferSlice.SlicePadStart(buffer, length: 0);

        Assert.AreEqual(string.Empty, Text(result));
    }
}