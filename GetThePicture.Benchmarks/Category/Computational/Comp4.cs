using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using GetThePicture.Picture;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Base.Clause.Items;

namespace GetThePicture.Benchmarks.Category.Computational;

[System.Diagnostics.CodeAnalysis.SuppressMessage("Performance", "CA1822:Mark members as static", Justification = "<Pending>")]
[BenchmarkCategory("COMP-4")]
public class Comp4Benchmark
{
    private static readonly PicMeta uintPic = PicMeta.Parse("9(18)");
    private static readonly byte[] uintBuffer = [0x0D, 0xE0, 0xB6, 0xB3, 0xA7, 0x63, 0xFF, 0xFF];
    private static readonly ulong uintValue = 999999999999999999L;

    private static readonly PicMeta intPic = PicMeta.Parse("S9(18)");
    private static readonly byte[] intBuffer = [0xF2, 0x1F, 0x49, 0x4C, 0x58, 0x9C, 0x00, 0x01];
    private static readonly long intValue = -999999999999999999L;
    
    
    private readonly Consumer consumer = new();
    
    [GlobalSetup]
    public void Setup()
    {
        uintPic.Usage = PicUsage.COMP4;
        intPic.Usage = PicUsage.COMP4;
    }

    [Benchmark]
    public void Comp4_Read_Integer()
    {
        var value = PicClauseCodec.ForMeta(uintPic).WithStrict().Decode(uintBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp4_Write_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(uintPic).WithStrict().Encode(uintValue);
        consumer.Consume(buffer);
    }

    [Benchmark]
    public void Comp4_Read_Signed_Integer()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp4_Write_Signed_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().Encode(intValue);
        consumer.Consume(buffer);
    }
}
