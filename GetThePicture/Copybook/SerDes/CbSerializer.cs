using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

public class CbSerializer
{
    private CbStorage _storage { get; }

    private readonly Dictionary<string, LeafNode> _flatMap;

    private CbRecord _record { get; set; } = null!;

    public CbSerializer(CbStorage storage)
    { 
        _storage = storage ?? throw new ArgumentNullException(nameof(storage));

        _flatMap = BuildFlatLeafMap(_storage);
    }

    public byte[] Exec(CbRecord record)
    {
        _record = record;

        // 1. 配置完整 buffer
        var buffer = new byte[_storage.TotalLength];

        // 2. 初始化 buffer
        Initialize(buffer);

        // 3. 寫入每個欄位
        WriteField(buffer, _record);

        return buffer;
    }

    private void WriteField(byte[] buffer, CbRecord record, string parentPath = "")
    {
        foreach (var field in record.Fields)
        {
            var path = string.IsNullOrEmpty(parentPath)
                ? field.Key
                : $"{parentPath}::{field.Key}";

            if (field.Value is CbRecord childRecord)
            {
                WriteField(buffer, childRecord, path);
            }
            else
            {
                if (!_flatMap.TryGetValue(path, out var node))
                    throw new InvalidOperationException($"Field '{path}' does not exist in storage map");

                if (node.Pic == null)
                    throw new InvalidOperationException($"PIC metadata missing for field '{path}'");

                if (field.Value == null)
                    throw new InvalidOperationException($"Value missing for field '{path}'");

                var bytes = PicClauseCodec.ForMeta(node.Pic).WithStrict().Encode(field.Value);

                if (node.StorageOccupied == null)
                    throw new InvalidOperationException($"StorageOccupied metadata missing for field '{path}'");

                var occupied = node.StorageOccupied.Value;

                if (bytes.Length != occupied)
                    throw new InvalidOperationException($"Field '{path}' encoded length mismatch. Expected {occupied}, got {bytes.Length}");

                Buffer.BlockCopy(bytes, 0, buffer, node.Offset, occupied);
            }
        }
    }

    private static void Initialize(byte[] buffer, byte value = 0x20)
    {
        // COBOL DISPLAY 預設 = SPACE
        // ASCII 系統通常是 0x20
        Array.Fill(buffer, value);
    }

    private static Dictionary<string, LeafNode> BuildFlatLeafMap(IStorageNode node)
    {
        var dict = new Dictionary<string, LeafNode>();

        void Walk(IStorageNode n, string parentPath)
        {
            // 如果有 Index (OCCURS)，就加上 (Index)
            string fieldName = n.Index.HasValue ? $"{n.Name}({n.Index.Value})" : n.Name;

            switch (n)
            {
                case CbStorage root:
                {
                    foreach (var child in root.Children)
                    {
                        if (child.IsAlias) continue;
                    
                        // COPYBOOK-STORAGE-MAP 要排除
                        Walk(child, string.Empty);
                    }
                    break;
                }

                case GroupNode group:
                {
                    var groupPath = string.IsNullOrEmpty(parentPath) ? fieldName : $"{parentPath}::{fieldName}";
                    
                    foreach (var child in group.Children)
                    {
                        if (child.IsAlias) continue;
                    
                        Walk(child, groupPath);
                    }
                        
                    break;
                }   

                case LeafNode leaf:
                    var leafPath = string.IsNullOrEmpty(parentPath) ? fieldName : $"{parentPath}::{fieldName}";

                    if (dict.ContainsKey(leafPath))
                        throw new InvalidOperationException($"Duplicate leaf path: {leafPath}");

                    dict.Add(leafPath, leaf);

                    break;

                default:
                    throw new InvalidOperationException($"Unsupported storage node type: {n.GetType().Name}");
            }
        }

        Walk(node, string.Empty);

        return dict;
    }
}
