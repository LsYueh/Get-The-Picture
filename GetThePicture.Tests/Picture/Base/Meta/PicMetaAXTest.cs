using GetThePicture.Picture.Base.Clause.Items;
using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Tests.Picture.Base.Meta;

[TestClass]
public class PicMetaAXTest
{
    // ─────────────────────────
    // Alphabetic (A)
    // ─────────────────────────

    [TestMethod]
    [DataRow(    "A", PicBaseClass.Alphabetic,  1, 0,  1, false)]
    [DataRow( "A(1)", PicBaseClass.Alphabetic,  1, 0,  1, false)]
    [DataRow("A(20)", PicBaseClass.Alphabetic, 20, 0, 20, false)]
    public void Parse_PIC_A(string symbols, PicBaseClass baseClass, int integerDigits, int decimalDigits, int digitCount, bool signed)
    {
        var pic = PicMeta.Parse(symbols);

        Assert.AreEqual(symbols      , pic.Raw);
        Assert.AreEqual(baseClass    , pic.BaseClass);
        Assert.AreEqual(integerDigits, pic.IntegerDigits);
        Assert.AreEqual(decimalDigits, pic.DecimalDigits);
        Assert.AreEqual(digitCount   , pic.DigitCount);
        Assert.AreEqual(signed       , pic.Signed);
    }
    
    // ─────────────────────────
    // Alphanumeric (X)
    // ─────────────────────────

    [TestMethod]
    [DataRow(       "X", PicBaseClass.Alphanumeric,  1, 0,  1, false)]
    [DataRow(      "XX", PicBaseClass.Alphanumeric,  2, 0,  2, false)]
    [DataRow(    "X(1)", PicBaseClass.Alphanumeric,  1, 0,  1, false)]
    [DataRow(   "X(20)", PicBaseClass.Alphanumeric, 20, 0, 20, false)]
    [DataRow("XXX(10)X", PicBaseClass.Alphanumeric, 13, 0, 13, false)]
    public void Parse_PIC_X(string symbols, PicBaseClass baseClass, int integerDigits, int decimalDigits, int digitCount, bool signed)
    {
        var pic = PicMeta.Parse(symbols);

        Assert.AreEqual(symbols      , pic.Raw);
        Assert.AreEqual(baseClass    , pic.BaseClass);
        Assert.AreEqual(integerDigits, pic.IntegerDigits);
        Assert.AreEqual(decimalDigits, pic.DecimalDigits);
        Assert.AreEqual(digitCount   , pic.DigitCount);
        Assert.AreEqual(signed       , pic.Signed);
    }
}
