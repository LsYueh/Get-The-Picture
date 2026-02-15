using GetThePicture.Picture.Clause.Base;

namespace GetThePicture.Picture.Clause.Decoder.Category.Numeric.Mapper;

public interface IMapper
{
    /// <summary>
    /// 將 decimal 數值映射到最合適的 CLR 整數型別
    /// </summary>
    object Map(decimal value, PicMeta pic);
}