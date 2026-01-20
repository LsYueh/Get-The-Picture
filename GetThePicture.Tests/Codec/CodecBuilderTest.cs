using System.Text;
using GetThePicture.Codec;
using GetThePicture.Codec.Utils;

namespace GetThePicture.Tests.Codec;

[TestClass]
public class CobolValueCodecTests
{
    // -------------------------
    // Length validation
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_LengthMismatch_ThrowsFormatException()
    {
        var pic = Pic.Parse("9(3)");

        byte[] buffer = Encoding.ASCII.GetBytes("1234");

        CodecBuilder.ForPic(pic).Decode(buffer);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_NumericWithNonDigit_ThrowsFormatException()
    {
        var pic = Pic.Parse("9(5)");

        byte[] buffer = Encoding.ASCII.GetBytes("1234");

        CodecBuilder.ForPic(pic).Decode(buffer);
    }
}