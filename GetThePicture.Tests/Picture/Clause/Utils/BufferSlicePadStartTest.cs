using System.Text;

using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Utils;

[TestClass]
public class BufferSlicePadStartTest
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
    public void InsufficientLength_ShouldPadStart()
    {
        // buffer = "123"
        var buffer = Bytes("123");

        var result = BufferSlice.SlicePadStart(buffer, length: 5);

        Assert.AreEqual("00123", Text(result));
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