using GetThePicture.Picture.Clause.Base;
using GetThePicture.Picture.Clause.Base.ClauseItems;

namespace GetThePicture.Copybook.Warpper.Base;

public readonly struct CbAddress
{
    /// <summary>欄位起始位置</summary>
    public int Start  { get; }
    
    /// <summary>欄位長度 (byte)</summary>
    public int Length { get; }
    
    /// <summary>欄位型別描述</summary>
    public PicMeta Meta { get; }

    /// <summary>
    /// Copybook 欄位位址資訊
    /// </summary>
    /// <param name="start">欄位在 buffer 的起始位置 (1-based)</param>
    /// <param name="length">欄位長度 (byte)</param>
    /// <param name="symbols">欄位的 PIC/型別描述</param>
    /// <param name="semantic">欄位的二次語意資料描述</param>
    /// <param name="usage">底層記憶體的儲存方式</param>
    public CbAddress(int start, int length, string symbols, PicSemantic semantic = PicSemantic.None, PicUsage usage = PicUsage.Display)
    {
        // Note: 內部使用 0-based
        Start = start - 1;

        Length = length;

        Meta = PicMetaBuilder.Parse(symbols, semantic, usage);

        if (Meta.StorageOccupied != Length)
            throw new ArgumentException($"Address length {Length} does not match PIC storage occupied {Meta.StorageOccupied}");
    }
}
