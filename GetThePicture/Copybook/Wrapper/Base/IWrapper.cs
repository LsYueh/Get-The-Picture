namespace GetThePicture.Copybook.Wrapper.Base;

/// <summary>
/// Copybook wrapper interface
/// </summary>
public interface IWrapper
{
    /// <summary>底層 byte buffer</summary>
    byte[] Raw { get; }

    /// <summary>透過欄位名稱讀寫資料</summary>
    object this[string name] { get; set; }
}
