using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

internal class CbSerializer
{
    public static byte[] SerSchema(CbSchema schema, RecValue record)
    {
        ArgumentNullException.ThrowIfNull(schema);
        ArgumentNullException.ThrowIfNull(record);

        using var ms = new MemoryStream();
        using var writer = new BinaryWriter(ms);

        WriteItem(writer, schema, record);

        return ms.ToArray();
    }

    private static void WriteItem(BinaryWriter writer, IDataItem item, RecValue current)
    {
        switch (item)
        {
            case CbSchema schema:
                WriteSchema(writer, schema, current);
                break;
            case GroupItem group:
                WriteGroup(writer, group, current);
                break;

            case ElementaryDataItem elementary:
                WriteElementary(writer, elementary, current);
                break;

            default:
                throw new NotSupportedException(
                    $"Unsupported IDataItem type: {item.GetType().Name}");
        }
    }

    private static void WriteSchema(BinaryWriter writer, CbSchema schema, RecValue current)
    {
        foreach (var dataItem in schema.DataItems)
        {
            WriteItem(writer, dataItem, current);
        }
    }
    
    private static void WriteGroup(BinaryWriter writer, GroupItem group, RecValue current)
    {
        if (group.Name != null)
        {
            if (current[group.Name] is not RecValue child)
                throw new Exception($"Group '{group.Name}' requires a RecValue.");

            current = child;
        }
        
        foreach (var subordinate in group.Subordinates)
        {
            WriteItem(writer, subordinate, current);
        }
    }

    private static void WriteElementary(BinaryWriter writer, ElementaryDataItem item, RecValue current)
    {
        var pic = item.Pic ?? throw new InvalidOperationException($"Elementary item '{item.Name}' has no PIC clause.");

        object? value = current[item.Name];

        byte[] bytes = (item.IsFiller == true)
            ? [.. Enumerable.Repeat((byte)' ', pic.StorageOccupied)]
            : PicClauseCodec.ForMeta(pic).WithStrict().Encode(value);

        writer.Write(bytes);
    }
}
