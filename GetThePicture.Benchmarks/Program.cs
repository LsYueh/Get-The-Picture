using System.Text;

using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;

using GetThePicture.Copybook.SerDes;
using GetThePicture.Copybook.SerDes.Provider;
using GetThePicture.Copybook.SerDes.Record;

using GetThePicture.Picture.Clause.Utils;

namespace GetThePicture.Benchmarks;

public class SerDesBenchmark
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;
    
    private DataProvider provider = null!;
    private CbSerDes serDes = null!;

    private byte[][] _records = default!;
    private CbRecord[] _objects = default!;

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
}

public class Program
{
    public static void Main(string[] args)
    {
        var summary = BenchmarkRunner.Run<SerDesBenchmark>();
    }
}
