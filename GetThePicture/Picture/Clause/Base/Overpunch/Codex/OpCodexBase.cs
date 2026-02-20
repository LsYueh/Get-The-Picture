namespace GetThePicture.Picture.Clause.Base.Overpunch.Codex;

/// <summary>
/// Overpunch Codex<br/>
/// Data Description Entry : <see href="https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html">USAGE Clause</see>
/// </summary>
public static class OpCodexBase
{
    /// <summary>
    /// Overpunch Value
    /// </summary>
    /// <param name="isNegative"></param>
    /// <param name="digit"></param>
    public static string OpVal(bool isNegative, char digit) => $"{(isNegative ? '-' : '+')}{digit}";
    
    /// <summary>
    /// -Dca, -Dcb, -Dcm, -Dcr
    /// </summary>
    public static readonly Dictionary<byte, string> OP_POSITIVE_01 = new()
    {
        { (byte)'0', OpVal(false, '0') },
        { (byte)'1', OpVal(false, '1') },
        { (byte)'2', OpVal(false, '2') },
        { (byte)'3', OpVal(false, '3') },
        { (byte)'4', OpVal(false, '4') },
        { (byte)'5', OpVal(false, '5') },
        { (byte)'6', OpVal(false, '6') },
        { (byte)'7', OpVal(false, '7') },
        { (byte)'8', OpVal(false, '8') },
        { (byte)'9', OpVal(false, '9') },
    };

    public static readonly Dictionary<string, byte> OP_POSITIVE_01_REVERSE = OP_POSITIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<byte, string> OP_POSITIVE_02 = new()
    {
        { (byte)'{', OpVal(false, '0') },
        { (byte)'A', OpVal(false, '1') },
        { (byte)'B', OpVal(false, '2') },
        { (byte)'C', OpVal(false, '3') },
        { (byte)'D', OpVal(false, '4') },
        { (byte)'E', OpVal(false, '5') },
        { (byte)'F', OpVal(false, '6') },
        { (byte)'G', OpVal(false, '7') },
        { (byte)'H', OpVal(false, '8') },
        { (byte)'I', OpVal(false, '9') },
    };

    public static readonly Dictionary<string, byte> OP_POSITIVE_02_REVERSE = OP_POSITIVE_02.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dca, -Dci, -Dcn
    /// </summary>
    public static readonly Dictionary<byte, string> OP_NEGATIVE_01 = new()
    {
        { (byte)'}', OpVal(true, '0') },
        { (byte)'J', OpVal(true, '1') },
        { (byte)'K', OpVal(true, '2') },
        { (byte)'L', OpVal(true, '3') },
        { (byte)'M', OpVal(true, '4') },
        { (byte)'N', OpVal(true, '5') },
        { (byte)'O', OpVal(true, '6') },
        { (byte)'P', OpVal(true, '7') },
        { (byte)'Q', OpVal(true, '8') },
        { (byte)'R', OpVal(true, '9') },
    };

    public static readonly Dictionary<string, byte> OP_NEGATIVE_01_REVERSE = OP_NEGATIVE_01.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcb
    /// </summary>
    public static readonly Dictionary<byte, string> OP_NEGATIVE_02 = new()
    {
        { (byte)'@', OpVal(true, '0') },
        { (byte)'A', OpVal(true, '1') },
        { (byte)'B', OpVal(true, '2') },
        { (byte)'C', OpVal(true, '3') },
        { (byte)'D', OpVal(true, '4') },
        { (byte)'E', OpVal(true, '5') },
        { (byte)'F', OpVal(true, '6') },
        { (byte)'G', OpVal(true, '7') },
        { (byte)'H', OpVal(true, '8') },
        { (byte)'I', OpVal(true, '9') },
    };

    public static readonly Dictionary<string, byte> OP_NEGATIVE_02_REVERSE = OP_NEGATIVE_02.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcm
    /// </summary>
    public static readonly Dictionary<byte, string> OP_NEGATIVE_03 = new()
    {
        { (byte)'p', OpVal(true, '0') },
        { (byte)'q', OpVal(true, '1') },
        { (byte)'r', OpVal(true, '2') },
        { (byte)'s', OpVal(true, '3') },
        { (byte)'t', OpVal(true, '4') },
        { (byte)'u', OpVal(true, '5') },
        { (byte)'v', OpVal(true, '6') },
        { (byte)'w', OpVal(true, '7') },
        { (byte)'x', OpVal(true, '8') },
        { (byte)'y', OpVal(true, '9') },
    };

    public static readonly Dictionary<string, byte> OP_NEGATIVE_03_REVERSE = OP_NEGATIVE_03.ToDictionary(kv => kv.Value, kv => kv.Key);

    /// <summary>
    /// -Dcr
    /// </summary>
    public static readonly Dictionary<byte, string> OP_NEGATIVE_04 = new()
    {
        { (byte)' ', OpVal(true, '0') }, // (space)
        { (byte)'!', OpVal(true, '1') },
        { (byte)'"', OpVal(true, '2') }, // (double-quote)
        { (byte)'#', OpVal(true, '3') },
        { (byte)'$', OpVal(true, '4') },
        { (byte)'%', OpVal(true, '5') },
        { (byte)'&', OpVal(true, '6') },
        { (byte)'\'',OpVal(true, '7') }, // (single-quote)
        { (byte)'(', OpVal(true, '8') },
        { (byte)')', OpVal(true, '9') },
    };

    public static readonly Dictionary<string, byte> OP_NEGATIVE_04_REVERSE = OP_NEGATIVE_04.ToDictionary(kv => kv.Value, kv => kv.Key);
}
