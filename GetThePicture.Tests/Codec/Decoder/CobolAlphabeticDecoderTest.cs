using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolAlphabeticDecoderTest
{
    [TestMethod]
    public void Decode_Alphabetic_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphabetic,
            IntegerDigits = 5
        }; // A(5)

        var result = CobolValueCodec.Decode("AbC  ", pic);

        Assert.AreEqual("AbC", result);
    }

    [TestMethod]
    public void Decode_Alphabetic_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphabetic,
            IntegerDigits = 5
        }; // A(5)

        var result = CobolValueCodec.Decode("AbC  fGh", pic, false);

        Assert.AreEqual("AbC", result);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Alphanumeric_ThrowsFormatException()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphabetic,
            IntegerDigits = 5
        };

        CobolValueCodec.Decode("AbC@ ", pic);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_Numeric_ThrowsFormatException()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphabetic,
            IntegerDigits = 5
        };

        CobolValueCodec.Decode("12345", pic);
    }

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_CP950_ThrowsFormatException()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphabetic,
            IntegerDigits = 7
        };

        CobolValueCodec.Decode("中文字 ", pic);
    }
}
