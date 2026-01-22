using GetThePicture.Copybook.Compiler.Base;

namespace GetThePicture.Copybook.Compiler;

public static class FilePreprocessor
{
    public static IReadOnlyList<DocumentLine> Process(StreamReader reader)
    {
        var lines = new List<DocumentLine>();
        int lineNumber = 1;

        while (!reader.EndOfStream)
        {
            var line = reader.ReadLine() ?? string.Empty;
            lines.Add(new DocumentLine(lineNumber++, line));
        }

        return lines;
    }
}
