using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause.Base;

[TestClass]
public class PicMeta9SV
{
    // ─────────────────────────
    // Numeric - Integer only
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_9_Default()
    {
        var pic = PicMeta.Parse("9");

        Assert.AreEqual("9", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_WithRepeat()
    {
        var pic = PicMeta.Parse("9(4)");

        Assert.AreEqual("9(4)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(4, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(4, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_WithRepeat_OnlyOnce()
    {
        var pic = PicMeta.Parse("9(1)");

        Assert.AreEqual("9(1)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_999()
    {
        var pic = PicMeta.Parse("999");

        Assert.AreEqual("999", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(3, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Signed
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_S9()
    {
        var pic = PicMeta.Parse("S9");

        Assert.AreEqual("S9", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.DigitCount);
        Assert.IsTrue(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_S9_WithRepeat()
    {
        var pic = PicMeta.Parse("S9(5)");

        Assert.AreEqual("S9(5)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(5, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(5, pic.DigitCount);
        Assert.IsTrue(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Decimal (V)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_9V9()
    {
        var pic = PicMeta.Parse("9V9");

        Assert.AreEqual("9V9", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(2, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_Repeat_V_Repeat()
    {
        var pic = PicMeta.Parse("9(3)V9(2)");

        Assert.AreEqual("9(3)V9(2)", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(5, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_999V99()
    {
        var pic = PicMeta.Parse("999V99");

        Assert.AreEqual("999V99", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(5, pic.DigitCount);
        Assert.IsFalse(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Signed + Decimal
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_S9V9()
    {
        var pic = PicMeta.Parse("S9V9");

        Assert.AreEqual("S9V9", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(2, pic.DigitCount);
        Assert.IsTrue(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_S9_Repeat_V_Repeat()
    {
        var pic = PicMeta.Parse("S9(5)V99");

        Assert.AreEqual("S9(5)V99", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(5, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(7, pic.DigitCount);
        Assert.IsTrue(pic.Signed);
    }
}
