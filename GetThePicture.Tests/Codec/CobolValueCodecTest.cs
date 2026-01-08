using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;

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

        CobolValueCodec.ForPic(pic).Decode("1234");
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_NumericWithNonDigit_ThrowsFormatException()
    {
        var pic = Pic.Parse("9(5)");

        CobolValueCodec.ForPic(pic).Decode("12A34");
    }
}