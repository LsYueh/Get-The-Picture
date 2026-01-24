namespace GetThePicture.Copybook.Base;

public class CobolLine(int lineNumber)
{
    /// <summary>
    /// 自動編號
    /// </summary>
    public int LineNumber { get; set; } = lineNumber;
    /// <summary>
    /// Column 1-6
    /// </summary>
    public string Sequence { get; set; } = string.Empty;
    /// <summary>
    /// Column 7
    /// </summary>
    public char Indicator { get; set; } = ' ';
    /// <summary>
    /// Column 8-11
    /// </summary>
    public string AreaA { get; set; } = string.Empty;
    /// <summary>
    /// Column 12-72
    /// </summary>
    public string AreaB { get; set; } = string.Empty;
    /// <summary>
    /// Column 73-80（可選）
    /// </summary>
    public string Remark { get; set; } = string.Empty;

    /// <summary>
    /// 空行或註解行
    /// </summary>
    public bool IsIgnored { get; set; } = false;

    /// <summary>
    /// AreaA + AreaB
    /// </summary>
    public string Line
    {
        get { return AreaA + AreaB; }
    }

    public override string ToString()
    {
        // 格式化成 Coding Sheet（固定欄位寬度）
        return $"{Sequence,-6}{Indicator}{AreaA,-4}{AreaB,-61}{Remark,-8}";
    }

    public static IReadOnlyList<CobolLine> FromStreamReader(StreamReader streamReader)
    {
        var lines = new List<CobolLine>();
        int lineNumber = 1;

        while (!streamReader.EndOfStream)
        {
            var rawLine = streamReader.ReadLine() ?? string.Empty;

            var cobolLine = Parse(rawLine, lineNumber++);

            // 忽略註解或空行
            if (cobolLine.IsIgnored) continue;

            lines.Add(cobolLine);
        }

        return lines;
    }

    private static CobolLine Parse(string rawLine, int lineNumber = 0)
    {
        var cobolLine = new CobolLine(lineNumber);

        // 空行 → 標記為忽略
        if (string.IsNullOrWhiteSpace(rawLine))
        {
            cobolLine.IsIgnored = true;
            return cobolLine;
        }

        // Column 1–6: Sequence
        cobolLine.Sequence = rawLine.Length >= 6 ? rawLine[..6] : string.Empty;

        // Column 7: Indicator
        cobolLine.Indicator = rawLine.Length >= 7 ? rawLine[6] : ' ';

        // 註解行 → 標記為忽略
        if (cobolLine.Indicator == '*')
        {
            cobolLine.IsIgnored = true;
            return cobolLine;
        }

        // Column 8–11: Area A
        cobolLine.AreaA = rawLine.Length >= 11 ? rawLine.Substring(7, 4) : string.Empty;

        // Column 12–72: Area B
        cobolLine.AreaB = rawLine.Length >= 12
            ? rawLine.Substring(11, Math.Min(61, rawLine.Length - 11))
            : string.Empty;

        // Column 73–80: Remark
        cobolLine.Remark = rawLine.Length >= 73
            ? rawLine.Substring(72, Math.Min(8, rawLine.Length - 72))
            : string.Empty;

        return cobolLine;
    }
}
