using GetThePicture.Cobol;

namespace GetThePicture.Tests.Cobol;

[TestClass]
public class PicTest
{
    // ─────────────────────────
    // Alphabetic (A)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_A_DefaultLength()
    {
        var pic = Pic.Parse("A");

        Assert.AreEqual(PicDataType.Alphabetic, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_A_ExplicitLength()
    {
        var pic = Pic.Parse("A(20)");

        Assert.AreEqual(PicDataType.Alphabetic, pic.DataType);
        Assert.AreEqual(20, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(20, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

     [TestMethod]
    public void Parse_PIC_A_ExplicitLength_OnlyOnce()
    {
        var pic = Pic.Parse("A(1)");

        Assert.AreEqual(PicDataType.Alphabetic, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }
    
    // ─────────────────────────
    // Alphanumeric (X)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_X_DefaultLength()
    {
        var pic = Pic.Parse("X");

        Assert.AreEqual(PicDataType.Alphanumeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_X_ExplicitLength()
    {
        var pic = Pic.Parse("X(20)");

        Assert.AreEqual(PicDataType.Alphanumeric, pic.DataType);
        Assert.AreEqual(20, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(20, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

     [TestMethod]
    public void Parse_PIC_X_ExplicitLength_OnlyOnce()
    {
        var pic = Pic.Parse("X(1)");

        Assert.AreEqual(PicDataType.Alphanumeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Integer only
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_9_Default()
    {
        var pic = Pic.Parse("9");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_WithRepeat()
    {
        var pic = Pic.Parse("9(4)");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(4, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(4, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_WithRepeat_OnlyOnce()
    {
        var pic = Pic.Parse("9(1)");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_999()
    {
        var pic = Pic.Parse("999");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(3, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Signed
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_S9()
    {
        var pic = Pic.Parse("S9");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(1, pic.TotalLength);
        Assert.IsTrue(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_S9_WithRepeat()
    {
        var pic = Pic.Parse("S9(5)");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(5, pic.IntegerDigits);
        Assert.AreEqual(0, pic.DecimalDigits);
        Assert.AreEqual(5, pic.TotalLength);
        Assert.IsTrue(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Decimal (V)
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_9V9()
    {
        var pic = Pic.Parse("9V9");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(2, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_9_Repeat_V_Repeat()
    {
        var pic = Pic.Parse("9(3)V9(2)");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(5, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_999V99()
    {
        var pic = Pic.Parse("999V99");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(3, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(5, pic.TotalLength);
        Assert.IsFalse(pic.Signed);
    }

    // ─────────────────────────
    // Numeric - Signed + Decimal
    // ─────────────────────────

    [TestMethod]
    public void Parse_PIC_S9V9()
    {
        var pic = Pic.Parse("S9V9");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(1, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(2, pic.TotalLength);
        Assert.IsTrue(pic.Signed);
    }

    [TestMethod]
    public void Parse_PIC_S9_Repeat_V_Repeat()
    {
        var pic = Pic.Parse("S9(5)V99");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(5, pic.IntegerDigits);
        Assert.AreEqual(2, pic.DecimalDigits);
        Assert.AreEqual(7, pic.TotalLength);
        Assert.IsTrue(pic.Signed);
    }

    // ─────────────────────────
    // Whitespace / Case
    // ─────────────────────────

    [TestMethod]
    public void Parse_LowerCase_WithSpaces()
    {
        var pic = Pic.Parse("    s9(2) v9 ");

        Assert.AreEqual(PicDataType.Numeric, pic.DataType);
        Assert.AreEqual(2, pic.IntegerDigits);
        Assert.AreEqual(1, pic.DecimalDigits);
        Assert.AreEqual(3, pic.TotalLength);
        Assert.IsTrue(pic.Signed);
    }

    // ─────────────────────────
    // Error handling
    // ─────────────────────────

    [TestMethod]
    public void Parse_Empty_ShouldThrow()
    {
        Assert.ThrowsException<ArgumentException>(() =>
            Pic.Parse(""));
    }

    [TestMethod]
    public void Parse_With_PIC_Keyword_ShouldThrow()
    {
        Assert.ThrowsException<NotSupportedException>(() =>
            Pic.Parse("PIC 9(3)"));
    }
}
