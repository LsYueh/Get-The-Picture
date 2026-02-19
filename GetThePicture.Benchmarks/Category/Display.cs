using System.Text;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Engines;

using GetThePicture.Picture.Clause;
using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Benchmarks.Category;

[BenchmarkCategory("DISPLAY")]
public class DisplayBenchmark
{
    private static readonly PicMeta uintPic = PicMeta.Parse("9(18)");
    private static readonly byte[] uintBuffer = Encoding.ASCII.GetBytes("999999999999999995");
    private static readonly ulong uintValue = 999999999999999995;

    private static readonly PicMeta intPic = PicMeta.Parse("S9(18)");
    private static readonly byte[] intBuffer = Encoding.ASCII.GetBytes("99999999999999999N");
    private static readonly long intValue = -999999999999999995;

    private static readonly PicMeta decPic = PicMeta.Parse("S9(3)V9(2)");
    private static readonly byte[] decBuffer = Encoding.ASCII.GetBytes("1234N");
    private static readonly decimal decValue = -123.45m;

    private readonly Consumer consumer = new();

    [Benchmark]
    public void Display_Read_Integer()
    {
        var value = PicClauseCodec.ForMeta(uintPic).WithStrict().Decode(uintBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Display_Write_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(uintPic).WithStrict().Encode(uintValue);
        consumer.Consume(buffer);
    }

        [Benchmark]
    public void Display_Read_Signed_Integer()
    {
        var value = PicClauseCodec.ForMeta(intPic).WithStrict().Decode(intBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Display_Write_Signed_Integer()
    {
        byte[] buffer = PicClauseCodec.ForMeta(intPic).WithStrict().Encode(intValue);
        consumer.Consume(buffer);
    }

    [Benchmark]
    public void Display_Read_Decimal()
    {
        var value = PicClauseCodec.ForMeta(decPic).WithStrict().Decode(decBuffer);
        consumer.Consume(value);
    }

    [Benchmark]
    public void Display_Write_Decimal()
    {
        byte[] buffer = PicClauseCodec.ForMeta(decPic).WithStrict().Encode(decValue);
        consumer.Consume(buffer);
    }
}