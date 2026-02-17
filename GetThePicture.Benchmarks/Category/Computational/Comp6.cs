using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Benchmarks.Category.Computational;

[System.Diagnostics.CodeAnalysis.SuppressMessage("Performance", "CA1822:Mark members as static", Justification = "<Pending>")]
[BenchmarkCategory("COMP-6")]
public class Comp6Benchmark
{
    private static readonly PicMeta intPic = PicMeta.Parse("9(10)");
    private static readonly byte[] intBuffer = [0x00, 0x00, 0x00, 0x00, 0x01];
    private static readonly ulong intValue = 1;

    private readonly Consumer consumer = new();
    
    [GlobalSetup]
    public void Setup()
    {
        intPic.Usage = PicUsage.COMP6;
    }

    [Benchmark]
    public void Comp6_Read_Integer()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp6_Write_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().Encode(intValue);
        consumer.Consume(buffer);
    }
}
