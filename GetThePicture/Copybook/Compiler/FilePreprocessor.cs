using GetThePicture.Copybook.Compiler.Base;

namespace GetThePicture.Copybook.Compiler;

public static class FilePreprocessor
{
    public static IReadOnlyList<CobolLine> Process(StreamReader reader)
    {
        var lines = new List<CobolLine>();
        int lineNumber = 1;

        while (!reader.EndOfStream)
        {
            var rawLine = reader.ReadLine() ?? string.Empty;

            var cobolLine = CobolLine.Parse(rawLine, lineNumber++);

            // 忽略註解或空行
            if (cobolLine.IsIgnored) continue;

            lines.Add(cobolLine);
        }

        return lines;
    }
}
