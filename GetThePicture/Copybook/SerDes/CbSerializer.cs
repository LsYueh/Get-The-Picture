using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

internal class CbSerializer
{
    public static byte[] SerSchema(CbSchema schema, CbRecord record)
    {
        ArgumentNullException.ThrowIfNull(schema);
        ArgumentNullException.ThrowIfNull(record);

        using var ms = new MemoryStream();
        using var writer = new BinaryWriter(ms);

        WriteGroupItems(writer, schema, record);

        return ms.ToArray();
    }

    private static void WriteGroupItems(BinaryWriter writer, IDataItem item, CbRecord current)
    {
        foreach (var child in item.Children)
        {
            switch (child)
            {
                case GroupItem g:
                    WriteNestedGroupItem(writer, g, current);
                    break;

                case ElementaryDataItem e:
                    WriteElementaryDataItem(writer, e, current);
                    break;

                default:
                    throw new NotSupportedException(
                        $"Unsupported IDataItem type: {item.GetType().Name}");
            }
        }
    }
    
    private static void WriteNestedGroupItem(BinaryWriter writer, GroupItem group, CbRecord current)
    {
        // 取得 GroupItem 的資料來源
        object? value = (group.Name != null ? current[group.Name] : current) ?? throw new Exception($"Group '{group.Name}' requires a CbRecord(s).");

        int occurs = group.Occurs ?? 1;

        if (occurs == 1)
        {
            if (value is not CbRecord record)
                throw new Exception($"Group '{group.Name}' requires a CbRecord.");

            WriteGroupItems(writer, group, record);
        }
        else
        {
            // OCCURS 對應 CbRecord[]
            
            if (value is not IReadOnlyList<CbRecord> records)
                throw new Exception($"Group '{group.Name}' OCCURS {occurs} TIMES requires a list.");

            if (records.Count != occurs)
                throw new Exception($"Group '{group.Name}' OCCURS {occurs} TIMES but got {records.Count}.");

            foreach (var record in records)
            {
                WriteGroupItems(writer, group, record);
            }
        }
    }

    private static void WriteElementaryDataItem(BinaryWriter writer, ElementaryDataItem item, CbRecord current)
    {
        var pic = item.Pic ?? throw new InvalidOperationException($"Elementary data item '{item.Name}' has no PIC clause.");

        int occurs = item.Occurs ?? 1;
        
        // FILLER
        if (item.IsFiller == true)
        {
            for (int i = 0; i < occurs; i++)
            {
                writer.Write(Enumerable.Repeat((byte)' ', pic.StorageOccupied).ToArray());
            }

            return;
        }

        // 取得 ElementaryDataItem 的資料來源
        object? value = current[item.Name] ?? throw new InvalidOperationException($"Elementary data item '{item.Name}' requires a Object(s).");

        if (occurs == 1)
        {
            byte[] bytes = PicClauseCodec.ForMeta(pic).WithStrict().Encode(value);

            writer.Write(bytes);
        }
        else
        {
            // OCCURS 對應 object[]

            if (value is not IReadOnlyList<object> values)
                throw new InvalidOperationException($"Elementary data item '{item.Name}' OCCURS {occurs} TIMES requires a list.");
        
            if (values.Count != occurs)
                throw new InvalidOperationException($"Elementary data item '{item.Name}' OCCURS {occurs} TIMES but got {values.Count}.");
        
            foreach (var v in values)
            {
                byte[] bytes = PicClauseCodec.ForMeta(pic).WithStrict().Encode(v);

                writer.Write(bytes);
            }
        }
    }
}
