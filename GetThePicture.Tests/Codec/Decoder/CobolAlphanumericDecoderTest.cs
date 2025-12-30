using GetThePicture.Cobol;
using GetThePicture.Codec;

namespace GetThePicture.Tests.Codec.Decoder;

[TestClass]
public class CobolAlphanumericDecoderTest
{
    [TestMethod]
    public void Decode_Alphanumeric_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphanumeric,
            IntegerDigits = 5
        }; // X(5)

        var result = CobolValueCodec.Build("ABC  ", pic).Decode();

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_Lesser_Extra_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphanumeric,
            IntegerDigits = 6
        }; // X(6)

        var result = CobolValueCodec.Build("ABC  ", pic).NoStrict().Decode();

        Assert.AreEqual("ABC", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphanumeric,
            IntegerDigits = 7
        }; // X(7)

        var result = CobolValueCodec.Build("中文字 ", pic).Decode();

        Assert.AreEqual("中文字", result);
    }

    [TestMethod]
    public void Decode_Alphanumeric_CP950_Lesser_TrimsRightSpaces()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Alphanumeric,
            IntegerDigits = 5
        }; // X(5)

        var result = CobolValueCodec.Build("中文字 ", pic).NoStrict().Decode();

        Assert.AreEqual("中文?", result);
    }
}
