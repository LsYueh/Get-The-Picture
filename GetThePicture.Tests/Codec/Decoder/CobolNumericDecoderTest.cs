using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolNumericDecoderTest
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

        var value = CobolValueCodec.Decode("00123", pic);

        Assert.AreEqual(123L, value);
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

        var result = CobolValueCodec.Decode("}0123", pic);

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

        var result = CobolValueCodec.Decode("0012L", pic);

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
            Signed = true,
            IntegerDigits = 3,
            DecimalDigits = 2
        };

        var result = CobolValueCodec.Decode("1234N", pic);

        Assert.AreEqual(-123.45m, result);
    }
}