using System.Text;

using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Provider;
using GetThePicture.Copybook.SerDes.Record;

using GetThePicture.Copybook.Warpper;
using GetThePicture.Copybook.Warpper.Base;

using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Benchmarks;

public class T30_t(byte[] raw) : CbWarpper(raw)
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

    // ...
}

public class SerDesBenchmark
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    private DataProvider provider = null!;
    private CbSerDes serDes = null!;

    private byte[][] _records = default!;
    private CbRecord[] _objects = default!;

    private T30_t[] _t30s = default!;

    [GlobalSetup]
    public void Setup()
    {
        provider = new DataProvider(new StreamReader(@"TestData/twse/t30-otc.cpy", cp950));
        serDes = new CbSerDes(provider);

        var lines = File.ReadAllLines(
            @"TestData/twse/t30-otc-lite.dat",
            cp950
        );

        // Deserialize benchmark 用
        _records = [.. lines.Select(l => cp950.GetBytes(l))];
        
        // Serialize benchmark 用
        _objects = new CbRecord[_records.Length];
        for (int i = 0; i < _records.Length; i++)
        {
            _objects[i] = serDes.Deserialize(_records[i]);
        }

        // Warpper benchmark 用
        _t30s = [.. _records.Select(r => new T30_t(r))];
    }

    [Benchmark]
    public void Deserialize()
    {
        foreach (var bytes in _records)
        {
            serDes.Deserialize(bytes);
        }
    }

    [Benchmark]
    public void Serialize()
    {
        foreach (var record in _objects)
        {
            serDes.Serialize(record);
        }
    }

    // Round-trip
    [Benchmark]
    public void Deserialize_Serialize()
    {
        for (int i = 0; i < _records.Length; i++)
        {
            var record = serDes.Deserialize(_records[i]);
            serDes.Serialize(record);
        }
    }
    
    [Benchmark]
    public void Warpper_Read()
    {
        long hash = 0;

        foreach (var t30 in _t30s)
        {
            var val = t30.StockNo;
            hash ^= val?.GetHashCode() ?? 0;
        }

        GC.KeepAlive(hash); // 避免優化
    }

    [Benchmark]
    public void Warpper_Write()
    {
        foreach (var t30 in _t30s)
        {
            t30.StockNo = "2330";
        }

        GC.KeepAlive(_t30s);
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        var summary = BenchmarkRunner.Run<SerDesBenchmark>();
    }
}
