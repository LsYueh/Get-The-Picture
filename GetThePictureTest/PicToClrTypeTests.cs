using GetThePicture;

namespace GetThePictureTest;

[TestClass]
public class PicToClrTypeTests
{
    // ─────────────────────────
    // Alphanumeric
    // ─────────────────────────

    [TestMethod]
    public void PIC_X_Should_Map_To_String()
    {
        var pic = Pic.Parse("X(10)");

        Assert.AreEqual(typeof(string), pic.ClrType);
    }

    // ─────────────────────────
    // Numeric - Integer
    // ─────────────────────────

    [TestMethod]
    public void PIC_9_Should_Map_To_Int()
    {
        var pic = Pic.Parse("9");

        Assert.AreEqual(typeof(int), pic.ClrType);
    }

    [TestMethod]
    public void PIC_9_Repeat_Should_Map_To_Int()
    {
        var pic = Pic.Parse("9(5)");

        Assert.AreEqual(typeof(int), pic.ClrType);
    }

    [TestMethod]
    public void PIC_S9_Should_Map_To_Int()
    {
        var pic = Pic.Parse("S9(4)");

        Assert.AreEqual(typeof(int), pic.ClrType);
    }

    // ─────────────────────────
    // Numeric - Decimal
    // ─────────────────────────

    [TestMethod]
    public void PIC_9V9_Should_Map_To_Decimal()
    {
        var pic = Pic.Parse("9V9");

        Assert.AreEqual(typeof(decimal), pic.ClrType);
    }

    [TestMethod]
    public void PIC_9_Repeat_V_Repeat_Should_Map_To_Decimal()
    {
        var pic = Pic.Parse("9(3)V9(2)");

        Assert.AreEqual(typeof(decimal), pic.ClrType);
    }

    [TestMethod]
    public void PIC_S9_With_Decimal_Should_Map_To_Decimal()
    {
        var pic = Pic.Parse("S9(5)V99");

        Assert.AreEqual(typeof(decimal), pic.ClrType);
    }

    // ─────────────────────────
    // Safety net
    // ─────────────────────────

    [TestMethod]
    public void DecimalDigits_GreaterThanZero_Always_Decimal()
    {
        var pic = new PicClause
        {
            DataType = PicDataType.Numeric,
            IntegerDigits = 1,
            DecimalDigits = 10
        };

        Assert.AreEqual(typeof(decimal), pic.ClrType);
    }
}