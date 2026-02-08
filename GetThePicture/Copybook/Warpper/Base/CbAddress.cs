using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Warpper.Base;

/// <summary>
/// Copybook 欄位位址資訊
/// </summary>
/// <param name="start">欄位在 buffer 的起始位置</param>
/// <param name="length">欄位長度 (byte)</param>
/// <param name="meta">欄位的 PIC/型別描述</param>
public readonly struct CbAddress(int start, int length, PicMeta meta)
{
    /// <summary>欄位起始位置</summary>
    public int Start  { get; } = start;
    
    /// <summary>欄位長度 (byte)</summary>
    public int Length { get; } = length;
    
    /// <summary>欄位型別描述</summary>
    public PicMeta Meta { get; } = meta;
}
