using GetThePicture.Cobol.Picture.OverpunchBase;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Tests.Cobol.Picture.OverpunchBase;

[TestClass]
public class OverpunchCodexTest
{
    [TestMethod]
    public void OverpunchCode_Positive_CI()
    {
        var OpCode = OverpunchCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue('C', out var info);

        Assert.AreEqual(1, info.Sign);
        Assert.AreEqual('3', info.Digit);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CI()
    {
        var OpCode = OverpunchCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue('}', out var info);

        Assert.AreEqual(-1, info.Sign);
        Assert.AreEqual('0', info.Digit);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CR()
    {
        var OpCode = OverpunchCodex.Map[DataStorageOptions.CR];

        OpCode.TryGetValue(' ', out var info_0);
        Assert.AreEqual(-1, info_0.Sign);
        Assert.AreEqual('0', info_0.Digit);

        OpCode.TryGetValue('"', out var info_2);
        Assert.AreEqual(-1, info_2.Sign);
        Assert.AreEqual('2', info_2.Digit);

        OpCode.TryGetValue((char)0x27, out var info_7); // Single quote is 39 in decimal (or 0x27 in hexadecimal)
        Assert.AreEqual(-1, info_7.Sign);
        Assert.AreEqual('7', info_7.Digit);
    }
}
