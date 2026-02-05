using System.Text;

using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Picture.Clause.Utils;

[TestClass]
public class BufferSlicePadEndTest
{
    private static byte[] Bytes(string s) => Encoding.ASCII.GetBytes(s);

    private static string Text(ReadOnlySpan<byte> span) => Encoding.ASCII.GetString(span);

    [TestMethod]
    public void ExactOrOverLength_ShouldSliceFromEnd()
    {
        var buffer = Bytes("ABCDEF");

        var result = BufferSlice.SlicePadEnd(buffer, length: 4);

        Assert.AreEqual("ABCD", Text(result));
    }

    [TestMethod]
    public void InsufficientLength_ShouldPadStart()
    {
        var buffer = Bytes("ABC");

        var result = BufferSlice.SlicePadEnd(buffer, length: 5);

        Assert.AreEqual("ABC  ", Text(result));
    }

    [TestMethod]
    public void ExactLength_ShouldReturnOriginal()
    {
        var buffer = Bytes("ABCDEF");

        var result = BufferSlice.SlicePadEnd(buffer, length: 6);

        Assert.AreEqual("ABCDEF", Text(result));
    }

    [TestMethod]
    public void LengthZero_ShouldReturnEmpty()
    {
        var buffer = Bytes("ABCDEF");

        var result = BufferSlice.SlicePadEnd(buffer, length: 0);

        Assert.AreEqual(string.Empty, Text(result));
    }
}