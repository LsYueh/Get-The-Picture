using System.Text;
using GetThePicture.Copybook.Wrapper;
using GetThePicture.Copybook.Wrapper.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Copybook.Wrapper;

public class T30_t(byte[]? raw = null) : CbWrapper(raw)
{
    // ----------------------------
    // Copybook Address Map
    // ----------------------------

    protected override Dictionary<string, CbAddress> AddressMap { get; } = new Dictionary<string, CbAddress>
    {
        ["STOCK-NO"]      = new CbAddress( 1, 6, "X(6)"),
        ["BULL-PRICE"]    = new CbAddress( 7, 9, "9(5)V9(4)"),
        ["LDC-PRICE"]     = new CbAddress(16, 9, "9(5)V9(4)"),
        ["BEAR-PRICE"]    = new CbAddress(25, 9, "9(5)V9(4)"),
        ["LAST-MTH-DATE"] = new CbAddress(34, 8, "9(8)", PicSemantic.GregorianDate),
        ["SETTYPE"]       = new CbAddress(42, 1, "X(01)"),
        ["MARK-W"]        = new CbAddress(43, 1, "X(01)"),
        ["MARK-P"]        = new CbAddress(44, 1, "X(01)"),
        ["MARK-L"]        = new CbAddress(45, 1, "X(01)"),
        ["IND-CODE"]      = new CbAddress(46, 2, "X(02)"),
        ["IND-SUB-CODE"]  = new CbAddress(48, 2, "X(02)"),
        ["MARK-M"]        = new CbAddress(50, 1, "X(01)"),
        ["STOCK-NAME"]    = new CbAddress(51,16, "X(16)"),
        // MARK-W
            ["MATCH-INTERVAL"] = new CbAddress(67, 3, "9(03)"),
            ["ORDER-LIMIT"]    = new CbAddress(70, 6, "9(06)"),
            ["ORDERS-LIMIT"]   = new CbAddress(76, 6, "9(06)"),
            ["PREPAY-RATE"]    = new CbAddress(82, 3, "9(03)"),
        ["MARK-S"]        = new CbAddress(85, 1, "X(01)"),
        ["STK-MARK"]      = new CbAddress(86, 1, "X(01)"),
        ["MARK-F"]        = new CbAddress(87, 1, "X(01)"),
        ["MARK-DAY-TRADE"]= new CbAddress(88, 1, "X(01)"),
        ["STK-CTGCD"]     = new CbAddress(89, 1, "X(01)"),
        ["FILLER"]        = new CbAddress(90,11, "X(11)"),
    };

    // ----------------------------
    // 強型別屬性
    // ----------------------------

    public string StockNo
    {
        get => this["STOCK-NO"].Get<string>();
        set => this["STOCK-NO"].Set(value);
    }

    public decimal BullPrice
    {
        get => this["BULL-PRICE"].Get<decimal>();
        set => this["BULL-PRICE"].Set(value);
    }

    public decimal LdcPrice
    {
        get => this["LDC-PRICE"].Get<decimal>();
        set => this["LDC-PRICE"].Set(value);
    }

    public decimal BearPrice
    {
        get => this["BEAR-PRICE"].Get<decimal>();
        set => this["BEAR-PRICE"].Set(value);
    }

    public DateOnly LastMthDate
    {
        get => this["LAST-MTH-DATE"].Get<DateOnly>();
        set => this["LAST-MTH-DATE"].Set(value);
    }

    // ...

    public string StockName
    {
        get => this["STOCK-NAME"].Get<string>();
        set => this["STOCK-NAME"].Set(value);
    }

    // ...
}

[TestClass]
public class CbWrapperTest
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    [TestMethod]
    [TestCategory("Demo")]
    public void Wrapper_T30_Test()
    {
        const string before = "11011 00106600000096950000087300020251219000000  0台泥一永        000000000000000000000 0           ";
        const string after  = "2330  00106600000096950000087300020251114000000  0台積電          000000000000000000000X0           ";
        
        byte[] raw = cp950.GetBytes(before);
        
        var T30 = new T30_t(raw);

        Assert.AreEqual("11011", T30.StockNo);
        Assert.AreEqual(106.6m, T30.BullPrice);
        Assert.AreEqual(96.95m, T30.LdcPrice);
        Assert.AreEqual(87.3m, T30.BearPrice);
        Assert.AreEqual(new DateOnly(2025, 12, 19), T30.LastMthDate);
        Assert.AreEqual("0", T30["SETTYPE"].Get<string>());
        Assert.AreEqual("0", T30["MARK-W"].Get<string>());
        Assert.AreEqual("0", T30["MARK-P"].Get<string>());
        Assert.AreEqual("0", T30["MARK-L"].Get<string>());
        Assert.AreEqual("00", T30["IND-CODE"].Get<string>());
        Assert.AreEqual("", T30["IND-SUB-CODE"].Get<string>());
        Assert.AreEqual("0", T30["MARK-M"].Get<string>());
        Assert.AreEqual("台泥一永", T30.StockName);
        Assert.AreEqual((UInt16) 0, T30["MATCH-INTERVAL"].Get<ushort>());
        Assert.AreEqual((UInt32) 0, T30["ORDER-LIMIT"].Get<uint>());
        Assert.AreEqual((UInt32) 0, T30["ORDERS-LIMIT"].Get<uint>());
        Assert.AreEqual((UInt16) 0, T30["PREPAY-RATE"].Get<ushort>());
        Assert.AreEqual("0", T30["MARK-S"].Get<string>());
        Assert.AreEqual("0", T30["STK-MARK"].Get<string>());
        Assert.AreEqual("0", T30["MARK-F"].Get<string>());
        Assert.AreEqual("", T30["MARK-DAY-TRADE"].Get<string>());
        Assert.AreEqual("0", T30["STK-CTGCD"].Get<string>());
        Assert.AreEqual("", T30["FILLER"].Get<string>());

        T30.StockNo = "2330";
        T30.LastMthDate = new DateOnly(2025, 11, 14);
        T30.StockName = "台積電";
        T30["MARK-DAY-TRADE"].Set("X");

        var str = cp950.GetString(T30.Raw);

        Assert.AreEqual(after, str);
    }

    [TestMethod]
    public void Wrapper_Default_Value()
    {
        var T30 = new T30_t();

        var str = cp950.GetString(T30.Raw);

        const string expected = "      00000000000000000000000000000000000                         000000000000000000                ";
        Assert.AreEqual(expected, str);
    }

    [TestMethod]
    [ExpectedException(typeof(KeyNotFoundException))]
    public void Wrapper_Get_Field_Throw_KeyNotFoundException()
    {
        const string s = "11011 00106600000096950000087300020251219000000  0台泥一永        000000000000000000000 0           ";
        
        byte[] raw = cp950.GetBytes(s);
        
        var T30 = new T30_t(raw);

        T30["SHOW-ME-THE-MONEY"].Get<uint>();
    }

    [TestMethod]
    [ExpectedException(typeof(KeyNotFoundException))]
    public void Wrapper_Set_Field_Throw_KeyNotFoundException()
    {
        const string s = "11011 00106600000096950000087300020251219000000  0台泥一永        000000000000000000000 0           ";
        
        byte[] raw = cp950.GetBytes(s);
        
        var T30 = new T30_t(raw);

        T30["SHOW-ME-THE-MONEY"].Set(100000000m);
    }
}