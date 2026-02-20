using System.Text;
using GetThePicture.Copybook.Wrapper;
using GetThePicture.Copybook.Wrapper.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;
using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Tests.Copybook.Wrapper;

public class T30_t(byte[] raw) : CbWrapper(raw)
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
        get => (string)this["STOCK-NO"]!;
        set => this["STOCK-NO"] = value;
    }

    public decimal BullPrice
    {
        get => (decimal)this["BULL-PRICE"]!;
        set => this["BULL-PRICE"] = value;
    }

    public decimal LdcPrice
    {
        get => (decimal)this["LDC-PRICE"]!;
        set => this["LDC-PRICE"] = value;
    }

    public decimal BearPrice
    {
        get => (decimal)this["BEAR-PRICE"]!;
        set => this["BEAR-PRICE"] = value;
    }

    public DateOnly LastMthDate
    {
        get => (DateOnly)this["LAST-MTH-DATE"]!;
        set => this["LAST-MTH-DATE"] = value;
    }

    // ...

    public string StockName
    {
        get => (string)this["STOCK-NAME"]!;
        set => this["STOCK-NAME"] = value;
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
        Assert.AreEqual("0", T30["SETTYPE"]);
        Assert.AreEqual("0", T30["MARK-W"]);
        Assert.AreEqual("0", T30["MARK-P"]);
        Assert.AreEqual("0", T30["MARK-L"]);
        Assert.AreEqual("00", T30["IND-CODE"]);
        Assert.AreEqual("", T30["IND-SUB-CODE"]);
        Assert.AreEqual("0", T30["MARK-M"]);
        Assert.AreEqual("台泥一永", T30.StockName);
        Assert.AreEqual((UInt16) 0, T30["MATCH-INTERVAL"]);
        Assert.AreEqual((UInt32) 0, T30["ORDER-LIMIT"]);
        Assert.AreEqual((UInt32) 0, T30["ORDERS-LIMIT"]);
        Assert.AreEqual((UInt16) 0, T30["PREPAY-RATE"]);
        Assert.AreEqual("0", T30["MARK-S"]);
        Assert.AreEqual("0", T30["STK-MARK"]);
        Assert.AreEqual("0", T30["MARK-F"]);
        Assert.AreEqual("", T30["MARK-DAY-TRADE"]);
        Assert.AreEqual("0", T30["STK-CTGCD"]);
        Assert.AreEqual("", T30["FILLER"]);

        T30.StockNo = "2330";
        T30.LastMthDate = new DateOnly(2025, 11, 14);
        T30.StockName = "台積電";
        T30["MARK-DAY-TRADE"] = "X";

        var str = cp950.GetString(T30.Raw);

         Assert.AreEqual(after, str);
    }
}