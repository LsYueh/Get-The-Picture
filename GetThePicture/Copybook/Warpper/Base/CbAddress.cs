using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Copybook.Warpper.Base;

/// <summary>
/// Copybook 欄位位址資訊
/// </summary>
/// <param name="start">欄位在 buffer 的起始位置 (1-based)</param>
/// <param name="length">欄位長度 (byte)</param>
/// <param name="symbols">欄位的 PIC/型別描述</param>
public readonly struct CbAddress
{
    /// <summary>欄位起始位置</summary>
    public int Start  { get; }
    
    /// <summary>欄位長度 (byte)</summary>
    public int Length { get; }
    
    /// <summary>欄位型別描述</summary>
    public PicMeta Meta { get; }

    public CbAddress(int start, int length, string symbols)
    {
        // Note: 內部使用 0-based
        Start = start - 1;

        Length = length;

        Meta = PicMetaBuilder.Parse(symbols);

        if (Meta.StorageOccupied != Length)
            throw new ArgumentException($"Address length {Length} does not match PIC storage occupied {Meta.StorageOccupied}");
    }
}
