using GetThePicture.PictureClause.Base;
using GetThePicture.PictureClause.Base.ClauseItems;

namespace GetThePicture.Tests.PictureClause.Base;

[TestClass]
public class PicMetaTest
{
    // ─────────────────────────
    // Whitespace / Case
    // ─────────────────────────

    [TestMethod]
    public void Parse_LowerCase_WithSpaces()
    {
        var pic = PicMeta.Parse("    s9(2) v9 ");

        Assert.AreEqual("S9(2)V9", pic.Raw);
        Assert.AreEqual(PicBaseClass.Numeric, pic.BaseClass);
        Assert.AreEqual(2, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(3, pic.DigitCount);
        Assert.IsTrue(pic.Signed);
    }

    // ─────────────────────────
    // Error handling
    // ─────────────────────────

    [TestMethod]
    public void Parse_Empty_ShouldThrow()
    {
        Assert.ThrowsException<ArgumentException>(() =>
            PicMeta.Parse(""));
    }

    [TestMethod]
    public void Parse_With_PIC_Keyword_ShouldThrow()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            PicMeta.Parse("PIC 9(3)"));
    }
}
