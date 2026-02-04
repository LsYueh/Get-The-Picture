using GetThePicture.Copybook.Compiler.Storage;
using GetThePicture.Copybook.Compiler.Storage.Base;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

public class CbDeserializer
{
    private CbStorage _storage { get; }

    private ReadOnlyMemory<byte> _buffer { get; set; }
    
    public CbDeserializer(CbStorage storage)
    {
        _storage = storage ?? throw new ArgumentNullException(nameof(storage));
    }
    
    /// <summary>
    /// Build Record Tree
    /// </summary>
    /// <param name="buffer"></param>
    /// <returns></returns>
    public CbRecord Exec(ReadOnlyMemory<byte> buffer)
    {
        _buffer = buffer;

        var result = ParseGroupNode(_storage);

        return result;
    }

    private CbRecord ParseGroupNode(IStorageNode node)
    {
        var record = new CbRecord();
        
        foreach (var child in node.Children)
        {
            // === COBOL FILLER：不輸出欄位 ===
            if (IsFiller(child))
            {
                // 仍然會走 ParseLeafNode 以確保 layout 正確
                if (child is LeafNode ln)
                    _ = ParseLeafNode(ln);

                continue;
            }
            
            // 如果有 Index (OCCURS)，就加上 (Index)
            string fieldName = child.Index.HasValue ? $"{child.Name}({child.Index.Value})" : child.Name;
                
            var childRecordOrValue = child switch
            {
                LeafNode  ln => ParseLeafNode(ln),
                GroupNode gn => ParseGroupNode(gn),
                _ => throw new InvalidOperationException($"Unknown StorageNode type: {child.GetType().Name}"),
            };

            record[fieldName] = childRecordOrValue;
        }

        return record;
    }

    private object ParseLeafNode(LeafNode node)
    {
        if (!node.StorageOccupied.HasValue)
            throw new InvalidOperationException("StorageOccupied is null");

        if (node.Pic is null)
            throw new InvalidOperationException($"PicMeta is not set for leaf '{node.Name}'");

        var raw = _buffer.Span.Slice(node.Offset, node.StorageOccupied.Value);
        object value = PicClauseCodec.ForMeta(node.Pic).Decode(raw);

        return value;
    }

    private static bool IsFiller(IStorageNode node) => string.Equals(node.Name, "FILLER", StringComparison.OrdinalIgnoreCase);
}