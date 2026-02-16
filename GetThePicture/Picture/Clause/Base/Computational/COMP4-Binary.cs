using GetThePicture.Picture.Clause.Base.Options;
using GetThePicture.Picture.Clause.Codec.Category.Numeric;

namespace GetThePicture.Picture.Clause.Base.Computational;

/// <summary>
/// COMP (Binary)
/// </summary>
internal static class COMP4
{
    // TRUNC example 1:
    //
    // 01  BIN-VAR     PIC S99 USAGE BINARY.
    //     MOVE 123451 to BIN-VAR
    //
    // +---------------------+---------+-------------+---------+
    // | Data item           | Decimal | Hex         | Display |
    // +---------------------+---------+-------------+---------+
    // | Sender              | 123451  | 00|01|E2|3B | 123451  |
    // | Receiver TRUNC(STD) | 51      | 00|33       | 51      | <<<
    // | Receiver TRUNC(OPT) | -7621   | E2|3B       | 2J      |
    // | Receiver TRUNC(BIN) | -7621   | E2|3B       | 762J    |
    // +---------------------+---------+-------------+---------+
    // 
    //
    // TRUNC example 2:
    //
    // 01  BIN-VAR     PIC 9(6)  USAGE BINARY
    //     MOVE 1234567891 to BIN-VAR
    //
    // +---------------------+------------+-------------+------------+
    // | Data item           | Decimal    | Hex         | Display    |
    // +---------------------+------------+-------------+------------+
    // | Sender              | 1234567891 | 49|96|02|D3 | 1234567891 |
    // | Receiver TRUNC(STD) | 567891     | 00|08|AA|53 | 567891     | <<<
    // | Receiver TRUNC(OPT) | 567891     | 53|AA|08|00 | 567891     |
    // | Receiver TRUNC(BIN) | 1234567891 | 49|96|02|D3 | 1234567891 |
    // +---------------------+------------+-------------+------------+
    //
    // https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=options-trunc

    /// <summary>
    /// COMP-4 is stored as Big Endian binary on mainframe systems. <br/>
    /// When running on Little Endian platforms (x86/x64), <br/>
    /// byte order must be reversed to maintain compatibility. <br/>
    /// </summary>
    private static BinaryOptions MainframeOption => BitConverter.IsLittleEndian ? BinaryOptions.Reversed : BinaryOptions.Normal;
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="buffer"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic)
    {
        // TODO: 只實作 TRUNC STD
        
        return COMP5.Decode(buffer, pic, MainframeOption);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="nMeta"></param>
    /// <param name="pic"></param>
    /// <returns></returns>
    public static byte[] Encode(NumericMeta nMeta, PicMeta pic)
    {
        // TODO: 只實作 TRUNC STD
       
        return COMP5.Encode(nMeta, pic, MainframeOption);
    }
}
