using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Base.Overpunch;

namespace GetThePicture.Tests.Picture.Clause.Base.Overpunch;

[TestClass]
public class OpCodexTest
{
    [TestMethod]
    public void OverpunchCode_Positive_CI()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue((byte)'C', out var info);

        Assert.AreEqual(1, info.Sign);
        Assert.AreEqual((byte)'3', info.Digit);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CI()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue((byte)'}', out var info);

        Assert.AreEqual(-1, info.Sign);
        Assert.AreEqual((byte)'0', info.Digit);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CR()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CR];

        OpCode.TryGetValue((byte)' ', out var info_0);
        Assert.AreEqual(-1, info_0.Sign);
        Assert.AreEqual((byte)'0', info_0.Digit);

        OpCode.TryGetValue((byte)'"', out var info_2);
        Assert.AreEqual(-1, info_2.Sign);
        Assert.AreEqual((byte)'2', info_2.Digit);

        OpCode.TryGetValue(0x27, out var info_7); // Single quote is 39 in decimal (or 0x27 in hexadecimal)
        Assert.AreEqual(-1, info_7.Sign);
        Assert.AreEqual((byte)'7', info_7.Digit);
    }
}
