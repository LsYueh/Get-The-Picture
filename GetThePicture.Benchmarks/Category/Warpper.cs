using System.Text;

using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;

using GetThePicture.Copybook.Wrapper;
using GetThePicture.Copybook.Wrapper.Base;

using GetThePicture.Picture.Clause.Utils;
using GetThePicture.TestData;

namespace GetThePicture.Benchmarks.Category;

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
        ["LAST-MTH-DATE"] = new CbAddress(34, 8, "9(8)"),
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

    /// <summary>
    /// STOCK-NO X(6) : 股票代號
    /// </summary>
    public string StockNo
    {
        get => this["STOCK-NO"].Get<string>();
        set => this["STOCK-NO"].Set(value);
    }

    /// <summary>
    /// BULL-PRICE 9(5)V9(4) : 漲停價
    /// </summary>
    public decimal BullPrice
    {
        get => this["BULL-PRICE"].Get<decimal>();
        set => this["BULL-PRICE"].Set(value);
    }

    /// <summary>
    /// LAST-MTH-DATE 9(8) : 上次成交日
    /// </summary>
    public uint LastMthDate
    {
        get => this["LAST-MTH-DATE"].Get<uint>();
        set => this["LAST-MTH-DATE"].Set(value);
    }
}

[BenchmarkCategory("Wrapper")]
public class WrapperBenchmark
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    private byte[][] _records = default!;

    private T30_t[] _t30s = default!;

    private readonly Consumer consumer = new();

    [GlobalSetup]
    public void Setup()
    {
        string filePath = TestFileProvider.GetPath("twse/t30-otc-lite.dat");

        var lines = File.ReadAllLines(filePath, cp950);

        _records = [.. lines.Select(l => cp950.GetBytes(l))];

        _t30s = [.. _records.Select(r => new T30_t(r))];
    }

    
    [Benchmark]
    public T30_t[] Wrapper_Read_String()
    {
        foreach (var t30 in _t30s)
        {
            consumer.Consume(t30.StockNo);
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }

    [Benchmark]
    public T30_t[] Wrapper_Write_String()
    {
        foreach (var t30 in _t30s)
        {
            t30.StockNo = "2330";
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }

    [Benchmark]
    public T30_t[] Wrapper_Read_Integer()
    {
        foreach (var t30 in _t30s)
        {
            consumer.Consume(t30.LastMthDate);
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }

    [Benchmark]
    public T30_t[] Wrapper_Write_Integer()
    {
        foreach (var t30 in _t30s)
        {
            t30.LastMthDate = 99999999U;
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }

    [Benchmark]
    public T30_t[] Wrapper_Read_Decimal()
    {
        foreach (var t30 in _t30s)
        {
            consumer.Consume(t30.BullPrice);
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }

    [Benchmark]
    public T30_t[] Wrapper_Write_Decimal()
    {
        foreach (var t30 in _t30s)
        {
            t30.BullPrice = 99999.9999m;
        }

        var result = _t30s;
        GC.KeepAlive(result);
        return result;
    }
}
