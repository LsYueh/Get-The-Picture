using GetThePicture.Cobol.Base;

namespace GetThePicture.Cobol;

public class CobolLine(int lineNumber, string rawText) : ICobolCodingSheet
{
    // ----------------------------
    // COBOL coding sheet
    // ----------------------------
    
    /// Column 1-6
    /// </summary>
    public string Sequence { get; private set; } = string.Empty;

    /// <summary>
    /// Column 7
    /// </summary>
    public char Indicator { get; private set; } = ' ';

    /// <summary>
    /// Column 8-11
    /// </summary>
    public string AreaA { get; private set; } = string.Empty;

    /// <summary>
    /// Column 12-72
    /// </summary>
    public string AreaB { get; private set; } = string.Empty;

    /// <summary>
    /// Column 73-80（可選）
    /// </summary>
    public string Remark { get; private set; } = string.Empty;

    // ----------------------------
    // COBOL Line
    // ----------------------------

    public string RawText { get; } = rawText;

    /// <summary>
    /// 自動編號
    /// </summary>
    public int LineNumber { get; set; } = lineNumber;
    /// <summary>

    /// <summary>
    /// 空行或註解行
    /// </summary>
    public bool IsIgnored { get; set; } = false;

    /// <summary>
    /// Area A / Area B
    /// </summary>
    public Area_t Area { get; private set;} = Area_t.None;

    /// <summary>
    /// AreaA + AreaB
    /// </summary>
    public string Text
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
            var rawText = streamReader.ReadLine() ?? string.Empty;

            var cobolLine = Parse(rawText, lineNumber++);

            // 忽略註解或空行
            if (cobolLine.IsIgnored) continue;

            lines.Add(cobolLine);
        }

        return lines;
    }

    private static CobolLine Parse(string rawText, int lineNumber = 0)
    {
        var cobolLine = new CobolLine(lineNumber, rawText);
        
        // 空行 → 標記為忽略
        if (string.IsNullOrWhiteSpace(rawText))
        {
            cobolLine.IsIgnored = true;
            return cobolLine;
        }

        // Column 1–6: Sequence
        cobolLine.Sequence = rawText.Length >= 6 ? rawText[..6] : string.Empty;

        // Column 7: Indicator
        cobolLine.Indicator = rawText.Length >= 7 ? rawText[6] : ' ';

        // 註解行 → 標記為忽略
        if (cobolLine.Indicator == '*')
        {
            cobolLine.IsIgnored = true;
            return cobolLine;
        }

        // Column 8–11: Area A
        cobolLine.AreaA = rawText.Length >= 11 ? rawText.Substring(7, 4) : string.Empty;

        // Column 12–72: Area B
        cobolLine.AreaB = rawText.Length >= 12
            ? rawText.Substring(11, Math.Min(61, rawText.Length - 11))
            : string.Empty;

        // Column 73–80: Remark
        cobolLine.Remark = rawText.Length >= 73
            ? rawText.Substring(72, Math.Min(8, rawText.Length - 72))
            : string.Empty;

        cobolLine.ClassifyArea();

        return cobolLine;
    }

    private void ClassifyArea()
    {
        if (!string.IsNullOrWhiteSpace(AreaA))
        {
            ValidateLevelIfNumeric(AreaA);
            Area = Area_t.A;
            return;
        }

        if (!string.IsNullOrWhiteSpace(AreaB))
        {
            Area = Area_t.B;
            return;
        }

        Area = Area_t.Free;
    }

    private void ValidateLevelIfNumeric(string areaA)
    {
        var text = areaA.Trim();

        // 不是純數字 → 不處理（可能是 SECTION / FD）
        if (!text.All(char.IsDigit))
            return;

        if (text.Length != 2)
            throw new FormatException(
                $"Line {LineNumber}: Level number must be 2 digits.");

        if (!int.TryParse(text, out int level))
            return;

        if (!IsValidLevel(level))
        {
            throw new FormatException(
                $"Line {LineNumber}: Invalid level number '{level}'.");
        }
    }

    /// <summary>
    /// 只做範圍檢查，進階判斷交給編譯器處理
    /// </summary>
    /// <param name="level"></param>
    /// <returns></returns>
    private static bool IsValidLevel(int level)
    {
        if (level >= 1 && level <= 49)
            return true;

        return level is 66 or 77 or 88;
    }
}
