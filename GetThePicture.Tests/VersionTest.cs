using System.Text.RegularExpressions;

namespace GetThePicture.Tests;

[TestClass]
public class VersionTest
{
    /// <summary>
    /// 測試 Informational Version 至少能讀到字串
    /// (MinVer 主要版本來源)
    /// </summary>
    [TestMethod]
    public void Informational_Should_Not_Be_Null_Or_Empty()
    {
        string version = Version.Informational;

        Assert.IsFalse(string.IsNullOrWhiteSpace(version), "Informational version should not be null or empty.");

        Console.WriteLine(version);
    }

    /// <summary>
    /// 測試 File Version 至少能讀到字串
    /// </summary>
    [TestMethod]
    public void FileVersion_Should_Not_Be_Null_Or_Empty()
    {
        string version = Version.File;

        Assert.IsFalse(string.IsNullOrWhiteSpace(version), "File version should not be null or empty.");

        Console.WriteLine(version);
    }

    /// <summary>
    /// 測試 Assembly Version 至少能讀到字串
    /// </summary>
    [TestMethod]
    public void AssemblyVersion_Should_Not_Be_Null_Or_Empty()
    {
        string version = Version.Assembly;

        Assert.IsFalse(string.IsNullOrWhiteSpace(version), "Assembly version should not be null or empty.");

        Console.WriteLine(version);
    }

    /// <summary>
    /// 額外測試：Informational Version 基本格式合理
    /// (允許 SemVer + prerelease + metadata)
    /// </summary>
    [TestMethod]
    public void Informational_Should_Look_Like_SemVer()
    {
        string version = Version.Informational;

        // SemVer 寬鬆驗證
        // 支援：
        // 1.2.3
        // 1.2.3-alpha
        // 1.2.3-alpha.1+build
        var semverPattern = @"^\d+\.\d+\.\d+([\-+].*)?$";

        Assert.IsTrue(Regex.IsMatch(version, semverPattern), $"Version '{version}' does not appear to be a valid semantic version.");
    }

}