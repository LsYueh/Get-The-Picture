using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;
using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Benchmarks.Category.Computational;

[System.Diagnostics.CodeAnalysis.SuppressMessage("Performance", "CA1822:Mark members as static", Justification = "<Pending>")]
[BenchmarkCategory("COMP-3")]
public class Comp3Benchmark
{
    private static readonly PicMeta intPic = PicMeta.Parse("9(10)");
    private static readonly byte[] intBuffer = [0x00, 0x00, 0x00, 0x00, 0x00, 0x1F];
    private static readonly ulong intValue = 1;
    
    private static readonly PicMeta decPic = PicMeta.Parse("S9(5)V9(2)");
    private static readonly byte[] decBuffer = [0x12, 0x34, 0x56, 0x7C];
    private static readonly decimal decValue = 12345.67m;

    private readonly Consumer consumer = new();
    
    [GlobalSetup]
    public void Setup()
    {
        intPic.Usage = PicUsage.COMP3;
        decPic.Usage = PicUsage.COMP3;
    }

    [Benchmark]
    public void Comp3_Read_Integer()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp3_Write_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().Encode(intValue);
        consumer.Consume(buffer);
    }

    [Benchmark]
    public void Comp3_Read_Decimal()
    {
        var value = PicClauseCodec.ForMeta(decPic).WithStrict().Decode(decBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Comp3_Write_Decimal()
    {
        byte[] buffer = PicClauseCodec.ForMeta(decPic).WithStrict().Encode(decValue);
        consumer.Consume(buffer);
    }
}
