using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;

namespace GetThePicture.Tests.PictureClause.Base;

[TestClass]
public class PicMetaAXTest
{
    // ─────────────────────────
    // Alphabetic (A)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_A_DefaultLength()
    {
        var pic = PicMeta.Parse("A");

        Assert.AreEqual("A", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphabetic, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_A_ExplicitLength()
    {
        var pic = PicMeta.Parse("A(20)");

        Assert.AreEqual("A(20)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphabetic, pic.BaseClass);
        Assert.AreEqual(20, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(20, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

     [TestMethod]
    public void Parse_PIC_A_ExplicitLength_OnlyOnce()
    {
        var pic = PicMeta.Parse("A(1)");

        Assert.AreEqual("A(1)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphabetic, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }
    
    // ─────────────────────────
    // Alphanumeric (X)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_X_DefaultLength()
    {
        var pic = PicMeta.Parse("X");

        Assert.AreEqual("X", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphanumeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_XX_DefaultLength()
    {
        var pic = PicMeta.Parse("XX");

        Assert.AreEqual("X", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphanumeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_X_ExplicitLength()
    {
        var pic = PicMeta.Parse("X(20)");

        Assert.AreEqual("X(20)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphanumeric, pic.BaseClass);
        Assert.AreEqual(20, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(20, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

     [TestMethod]
    public void Parse_PIC_X_ExplicitLength_OnlyOnce()
    {
        var pic = PicMeta.Parse("X(1)");

        Assert.AreEqual("X(1)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Alphanumeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }
}
