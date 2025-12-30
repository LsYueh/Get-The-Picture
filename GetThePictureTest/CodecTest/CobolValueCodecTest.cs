using GetThePicture.Codec;
using GetThePicture.Pic;

namespace GetThePictureTest.CodecTest;

[TestClass]
public class CobolValueCodecTests
{
    // -------------------------
    // Numeric - Unsigned Integer
    // -------------------------

    [TestMethod]
    public void Decode_UnsignedInteger_ReturnsLong()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 5,
            DecimalDigits = 0
        };

        var result = CobolValueCodec.Decode("00123", pic);

        Assert.AreEqual(123L, result);
    }

    // -------------------------
    // Numeric - Signed Integer
    // -------------------------

    [TestMethod]
    public void Decode_SignedInteger_LeadingSign()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = true,
            IntegerDigits = 5,
            DecimalDigits = 0
        };

        var result = CobolValueCodec.Decode("-00123", pic);

        Assert.AreEqual(-123L, result);
    }

    [TestMethod]
    public void Decode_SignedInteger_TrailingSign()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = true,
            IntegerDigits = 5,
            DecimalDigits = 0
        };

        var result = CobolValueCodec.Decode("00123-", pic);

        Assert.AreEqual(-123L, result);
    }

    // -------------------------
    // Numeric - Decimal (V)
    // -------------------------

    [TestMethod]
    public void Decode_Decimal_ReturnsDecimal()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 3,
            DecimalDigits = 2
        };

        var result = CobolValueCodec.Decode("12345", pic);

        Assert.AreEqual(123.45m, result);
    }

    [TestMethod]
    public void Decode_SignedDecimal_ReturnsNegativeDecimal()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            Signed = false,
            IntegerDigits = 3,
            DecimalDigits = 2
        };

        var result = CobolValueCodec.Decode("-12345", pic);

        Assert.AreEqual(-123.45m, result);
    }

    // -------------------------
    // Alphanumeric
    // -------------------------

    [TestMethod]
    public void Decode_Alphanumeric_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphanumeric,
            IntegerDigits = 5
        };

        var result = CobolValueCodec.Decode("ABC  ", pic);

        Assert.AreEqual("ABC", result);
    }

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
}