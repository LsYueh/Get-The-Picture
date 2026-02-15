using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Tests.Picture.Clause.Decoder.Category;

[TestClass]
public class NumericDecoderTest
{
    // -------------------------
    // Exceptions
    // -------------------------

    [DataTestMethod] // 總共 29 位數，超過 decimal 精度
    [DataRow("S9(29)")]
    [DataRow("9(29)")]
    [ExpectedException(typeof(OverflowException))]
    public void Decode_With_Digits_Exceeding_28_Should_Throw(string picString)
    {
        var pic = PicMeta.Parse(picString);

        PicClauseCodec.ForMeta(pic).Decode([(byte)'0']);
    }
}