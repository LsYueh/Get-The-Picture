namespace GetThePicture.Copybook.Compiler.Storage.Base;
public sealed class StorageAlias(IStorageNode target)
{
    public IStorageNode Target { get; } = target;
}
