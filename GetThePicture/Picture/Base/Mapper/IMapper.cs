using GetThePicture.Picture.Base.Meta;

namespace GetThePicture.Picture.Base.Mapper;

public interface IMapper
{
    /// <summary>
    /// 將 decimal 數值映射到最合適的 CLR 整數型別
    /// </summary>
    object Map(decimal value, PicMeta pic);
}