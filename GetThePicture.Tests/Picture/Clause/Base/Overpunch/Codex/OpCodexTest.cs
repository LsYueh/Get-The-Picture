using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Base.Overpunch.Codex;

namespace GetThePicture.Tests.Picture.Clause.Base.Overpunch.Codex;

[TestClass]
public class OpCodexTest
{
    [TestMethod]
    public void OverpunchCode_Positive_CI()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue((byte)'C', out var opValue);

        Assert.AreEqual("+3", opValue);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CI()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CI];

        OpCode.TryGetValue((byte)'}', out var opValue);

        Assert.AreEqual("-0", opValue);
    }

    [TestMethod]
    public void OverpunchCode_Negative_CR()
    {
        var OpCode = OpCodex.Map[DataStorageOptions.CR];

        OpCode.TryGetValue((byte)' ', out var opValue_0);
        Assert.AreEqual("-0", opValue_0);

        OpCode.TryGetValue((byte)'"', out var opValue_2);
        Assert.AreEqual("-2", opValue_2);

        OpCode.TryGetValue(0x27, out var opValue_7); // Single quote is 39 in decimal (or 0x27 in hexadecimal)
        Assert.AreEqual("-7", opValue_7);
    }
}
