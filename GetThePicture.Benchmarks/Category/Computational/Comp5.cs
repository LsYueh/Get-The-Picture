using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Benchmarks.Category.Computational;

[System.Diagnostics.CodeAnalysis.SuppressMessage("Performance", "CA1822:Mark members as static", Justification = "<Pending>")]
[BenchmarkCategory("COMP-5")]
public class Comp5Benchmark
{
    private static readonly PicMeta intPic = PicMeta.Parse("S9(18)");
    private static readonly byte[] intBuffer = [0xF2, 0x1F, 0x49, 0x4C, 0x58, 0x9C, 0x00, 0x01];
    private static readonly long intValue = -999999999999999999L;
    
    
    private readonly Consumer consumer = new();
    
    [GlobalSetup]
    public void Setup()
    {
        intPic.Usage = PicUsage.COMP5;
    }

    [Benchmark]
    public void Comp5_Read_Integer_BE()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp5_Write_Integer_BE()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().Encode(intValue);
        consumer.Consume(buffer);
    }

        [Benchmark]
    public void Comp5_Read_Integer_LE()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().IsLittleEndian().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp5_Write_Integer_LE()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().IsLittleEndian().Encode(intValue);
        consumer.Consume(buffer);
    }
}
