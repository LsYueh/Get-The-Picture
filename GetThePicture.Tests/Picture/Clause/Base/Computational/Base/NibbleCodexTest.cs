using GetThePicture.Picture.Clause.Base.Computational.Base;
using GetThePicture.Picture.Clause.Base.Options;

namespace GetThePicture.Tests.Picture.Clause.Base.Computational.Base;

[TestClass]
public class NibbleCodexTest
{
    [TestMethod]
    public void Nibble_Sign_CI()
    {
        var Nibbles = NibbleCodex.Map[DataStorageOptions.CI];

        Nibbles.TryGetValue(NibbleSign.Positive, out var BYTE_01);
        Nibbles.TryGetValue(NibbleSign.Negative, out var BYTE_02);
        Nibbles.TryGetValue(NibbleSign.Unsigned, out var BYTE_03);

        Assert.AreEqual((byte) 0x0C, BYTE_01);
        Assert.AreEqual((byte) 0x0D, BYTE_02);
        Assert.AreEqual((byte) 0x0F, BYTE_03);
    }

    [TestMethod]
    public void Nibble_Sign_CI_Rev()
    {
        var NibblesRev = NibbleCodex.ReversedMap[DataStorageOptions.CI];

        NibblesRev.TryGetValue(0x0C, out var NIBBLE_01);
        NibblesRev.TryGetValue(0x0D, out var NIBBLE_02);
        NibblesRev.TryGetValue(0x0F, out var NIBBLE_03);

        Assert.AreEqual(NibbleSign.Positive, NIBBLE_01);
        Assert.AreEqual(NibbleSign.Negative, NIBBLE_02);
        Assert.AreEqual(NibbleSign.Unsigned, NIBBLE_03);
    }
}