using GetThePicture.Picture;
using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Tests.Picture.Codec.Category.Numeric;

[TestClass]
public class DecoderTest
{
    // -------------------------
    // Exceptions
    // -------------------------

    [TestMethod] // 總共 29 位數，超過 decimal 精度
    [DataRow("S9(29)")]
    [DataRow("9(29)")]
    public void Decode_With_Digits_Exceeding_28_Should_Throw(string picString)
    {
        var pic = PicMeta.Parse(picString);

        Assert.ThrowsExactly<OverflowException>(() => PicClauseCodec.ForMeta(pic).Decode([(byte)'0']));
    }
}