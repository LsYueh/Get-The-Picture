using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Picture.Clause.Codec.Semantic;

public sealed class ValidationResult
{
    public bool IsValid => Errors.Count == 0;
    public List<string> Errors { get; } = new();
}

/// <summary>
/// 語意約束模型
/// </summary>
public sealed class Constraint
{
    public PicBaseClass[] AllowedClasses { get; init; } = [];

    public PicUsage[] AllowedUsage { get; init; } = [];

    public int? RequiredLength { get; init; }

    public int? RequiredDecimalDigits { get; init; }

    public bool? MustBeSigned { get; init; }

    /// <summary>
    /// 語意「合法結構」
    /// </summary>
    /// <param name="pic"></param>
    /// <returns></returns>
    public bool IsStructureValid(PicMeta pic)
    {
        if (AllowedClasses.Length > 0 && !AllowedClasses.Contains(pic.BaseClass))
            return false;

        if (AllowedUsage.Length > 0 && !AllowedUsage.Contains(pic.Usage))
            return false;

        if (RequiredLength is int len && pic.StorageOccupied != len)
            return false;

        if (RequiredDecimalDigits is int d && pic.DecimalDigits != d)
            return false;

        if (MustBeSigned is bool s && pic.Signed != s)
            return false;

        return true;
    }

    public void ValidateOrThrow(PicMeta pic, string semanticName)
    {
        if (AllowedClasses.Length > 0 && !AllowedClasses.Contains(pic.BaseClass))
            throw new NotSupportedException($"{semanticName} does not support BaseClass '{pic.BaseClass}'.");

        if (AllowedUsage.Length > 0 && !AllowedUsage.Contains(pic.Usage))
            throw new NotSupportedException($"{semanticName} does not support usage '{pic.Usage}'.");

        if (RequiredLength is int len && pic.StorageOccupied != len)
            throw new NotSupportedException($"{semanticName} must occupy exactly {len} bytes. Actual: {pic.StorageOccupied}");

        if (RequiredDecimalDigits is int d && pic.DecimalDigits != d)
            throw new NotSupportedException($"{semanticName} requires DecimalDigits = {d}. Actual: {pic.DecimalDigits}");

        if (MustBeSigned is bool s && pic.Signed != s)
            throw new NotSupportedException($"{semanticName} requires Signed = {s}.");
    }
}
