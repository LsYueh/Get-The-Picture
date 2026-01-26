using GetThePicture.Cobol.Meta;
using GetThePicture.Cobol.Picture.TypeBase;

namespace GetThePicture.Cobol.Picture.ComputationalBase;

/// <summary>
/// COMP (binary)
/// </summary>
internal static class COMP
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
    
    public static object Decode(ReadOnlySpan<byte> buffer, PicMeta pic, BinaryOptions endian = BinaryOptions.Normal)
    {
        // TODO: 只實作 TRUNC STD
        
        return COMP5.Decode(buffer, pic, endian);
    }

    public static byte[] Encode(CobMeta meta, PicMeta pic, BinaryOptions endian = BinaryOptions.Normal)
    {
       // TODO: 只實作 TRUNC STD
       
       return COMP5.Encode(meta, pic, endian);
    }
}
