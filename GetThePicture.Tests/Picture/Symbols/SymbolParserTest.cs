using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Symbols;
using GetThePicture.Picture.Symbols.Base;

namespace GetThePicture.Tests.Picture.Symbols;

[TestClass]
public class SymbolParserTest
{
    [DataTestMethod]
    [DataRow("X",        PicBaseClass.Alphanumeric, false,  1, 0)]
    [DataRow("XX",       PicBaseClass.Alphanumeric, false,  2, 0)]
    [DataRow("X(3)",     PicBaseClass.Alphanumeric, false,  3, 0)]
    [DataRow("X(1)",     PicBaseClass.Alphanumeric, false,  1, 0)]
    [DataRow("XX(2)",    PicBaseClass.Alphanumeric, false,  3, 0)]
    [DataRow("X(2)X(3)", PicBaseClass.Alphanumeric, false,  5, 0)]
    [DataRow("XXX(1)",   PicBaseClass.Alphanumeric, false,  3, 0)]
    [DataRow("X(93)",    PicBaseClass.Alphanumeric, false, 93, 0)]

    [DataRow("9",        PicBaseClass.Numeric, false,  1,  0)]
    [DataRow("99",       PicBaseClass.Numeric, false,  2,  0)]
    [DataRow("9(3)",     PicBaseClass.Numeric, false,  3,  0)]
    [DataRow("9(9)",     PicBaseClass.Numeric, false,  9,  0)]
    [DataRow("9(10)",    PicBaseClass.Numeric, false, 10,  0)]
    [DataRow("99(2)",    PicBaseClass.Numeric, false,  3,  0)] // 9 + 9(2)
    [DataRow("9(2)9(3)", PicBaseClass.Numeric, false,  5,  0)]
    [DataRow("9V9",      PicBaseClass.Numeric, false,  1,  1)]
    [DataRow("99V9",     PicBaseClass.Numeric, false,  2,  1)]
    [DataRow("99V9(10)9",PicBaseClass.Numeric, false,  2, 11)]
    [DataRow("9V99",     PicBaseClass.Numeric, false,  1,  2)]
    [DataRow("9(2)V9(3)",PicBaseClass.Numeric, false,  2,  3)]
    [DataRow("99V9(2)",  PicBaseClass.Numeric, false,  2,  2)]

    [DataRow("S9",       PicBaseClass.Numeric,  true,  1,  0)]
    [DataRow("S99",      PicBaseClass.Numeric,  true,  2,  0)]
    [DataRow("S9V9",     PicBaseClass.Numeric,  true,  1,  1)]
    [DataRow("S9(2)V9",  PicBaseClass.Numeric,  true,  2,  1)]
    public void Parser_Tests_01(string symbols, PicBaseClass baseClass, bool signed, int integerDigits, int decimalDigits)
    {
        SymbolMeta meta = SymbolParser.Read(symbols);

        Assert.AreEqual(baseClass    , meta.BaseClass);
        Assert.AreEqual(signed       , meta.Signed);
        Assert.AreEqual(integerDigits, meta.IntegerDigits);
        Assert.AreEqual(decimalDigits, meta.DecimalDigits);
    }

    [DataTestMethod]
    [DataRow("9P",       PicBaseClass.Numeric, false, 1, 0)]
    [DataRow("9P9",      PicBaseClass.Numeric, false, 2, 0)]
    [DataRow("9(2)P9",   PicBaseClass.Numeric, false, 3, 0)]
    [DataRow("SP9",      PicBaseClass.Numeric, true , 1, 0)]
    public void Parser_Tests_02(string symbols, PicBaseClass baseClass, bool signed, int integerDigits, int decimalDigits)
    {
        Assert.ThrowsException<NotSupportedException>(() => SymbolParser.Read(symbols));
    }

    [TestMethod]
    public void Parser_Should_Throw_When_Mixing_Classes()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("X9"));
    }

    [TestMethod]
    public void Parser_Should_Throw_When_Sign_With_Alpha()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("SX"));
    }

    [TestMethod]
    public void Parser_Should_Throw_On_Mixed_Classes()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("X9"));
    }

    [TestMethod]
    public void Parser_Should_Throw_On_Sign_With_Alpha()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("SX"));
    }

    [TestMethod]
    public void Parser_Should_Throw_On_V_Without_Numeric()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("V9"));
    }

    [TestMethod]
    public void Parser_Should_Throw_On_Multiple_V()
    {
        Assert.ThrowsException<Exception>(() => SymbolParser.Read("9V9V9"));
    }
}
