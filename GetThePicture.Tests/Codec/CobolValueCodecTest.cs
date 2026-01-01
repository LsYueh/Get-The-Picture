using GetThePicture.Cobol;
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
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 3,
            DecimalDigits = 0
        };

        CobolValueCodec.Decode("1234", pic);
    }

    // -------------------------
    // Invalid format
    // -------------------------

    [TestMethod]
    [ExpectedException(typeof(FormatException))]
    public void Decode_NumericWithNonDigit_ThrowsFormatException()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 5,
            DecimalDigits = 0
        };

        CobolValueCodec.Decode("12A34", pic);
    }

    [TestMethod]
    [ExpectedException(typeof(NotSupportedException))]
    public void Decode_Maximum_Supported_Length_Exceeded()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 30,
            DecimalDigits = 0
        };

        CobolValueCodec.Decode("123456789012345678901234567890", pic);
    }
}