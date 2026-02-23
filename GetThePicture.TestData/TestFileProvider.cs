namespace GetThePicture.TestData;

public static class TestFileProvider
{
    private static readonly string BasePath =
        Path.GetDirectoryName(typeof(TestFileProvider).Assembly.Location)!;

    public static string GetPath(string fileName)
        => Path.Combine(BasePath, "Files", fileName);

    public static string Read(string fileName)
        => File.ReadAllText(GetPath(fileName));
}