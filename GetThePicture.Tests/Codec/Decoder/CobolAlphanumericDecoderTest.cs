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
        };

        var result = CobolValueCodec.Decode("ABC  ", pic);

        Assert.AreEqual("ABC", result);
    }
    
}