using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Tests.Picture.Clause.Base;

[TestClass]
public class PicMeta9SV
{
    // ─────────────────────────
    // Numeric - Integer only
    // ─────────────────────────
    [DataTestMethod]
    [DataRow(   "9", PicBaseClass.Numeric, 1, 0, 1, false)]
    [DataRow("9(4)", PicBaseClass.Numeric, 4, 0, 4, false)]
    [DataRow("9(1)", PicBaseClass.Numeric, 1, 0, 1, false)]
    [DataRow( "999", PicBaseClass.Numeric, 3, 0, 3, false)]
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
    // Numeric - Signed
    // ─────────────────────────
    [DataTestMethod]
    [DataRow(   "S9", PicBaseClass.Numeric, 1, 0, 1, true)]
    [DataRow("S9(5)", PicBaseClass.Numeric, 5, 0, 5, true)]
    public void Parse_PIC_S9(string symbols, PicBaseClass baseClass, int integerDigits, int decimalDigits, int digitCount, bool signed)
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
    // Numeric - Decimal (V)
    // ─────────────────────────
    [DataTestMethod]
    [DataRow(      "9V9", PicBaseClass.Numeric, 1, 1, 2, false)]
    [DataRow("9(3)V9(2)", PicBaseClass.Numeric, 3, 2, 5, false)]
    [DataRow(   "999V99", PicBaseClass.Numeric, 3, 2, 5, false)]
    public void Parse_PIC_9V9(string symbols, PicBaseClass baseClass, int integerDigits, int decimalDigits, int digitCount, bool signed)
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
    // Numeric - Signed + Decimal
    // ─────────────────────────
    [DataTestMethod]
    [DataRow(    "S9V9", PicBaseClass.Numeric, 1, 1, 2, true)]
    [DataRow("S9(5)V99", PicBaseClass.Numeric, 5, 2, 7, true)]
    public void Parse_PIC_S9V9(string symbols, PicBaseClass baseClass, int integerDigits, int decimalDigits, int digitCount, bool signed)
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
