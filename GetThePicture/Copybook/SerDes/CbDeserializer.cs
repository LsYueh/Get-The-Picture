using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

internal class CbDeserializer
{
    internal static CbRecord DesSchema(CbSchema schema, ref RecCursor cursor)
    {
        var result = new CbRecord();

        foreach (var child in schema.Children)
        {
            switch (child)
            {
                case GroupItem g:
                    DesNestedGroupItem(g, ref cursor, result);
                    break;

                case ElementaryDataItem e :
                    DesElementaryDataItem(e, ref cursor, result);
                    break;
                    
                default:
                    throw new InvalidOperationException($"Unsupported data item type: {child.GetType().Name}");
            };
        }

        return result;
    }

    private static CbRecord DesGroupItem(GroupItem item, ref RecCursor cursor)
    {
        var result = new CbRecord();

        foreach (var child in item.Children)
        {
            switch (child)
            {
                case GroupItem g:
                    DesNestedGroupItem(g, ref cursor, result);
                    break;

                case ElementaryDataItem e :
                    DesElementaryDataItem(e, ref cursor, result);
                    break;

                default:
                    throw new InvalidOperationException($"Unsupported subordinate type: {child.GetType().Name}");
            }
        }

        return result;
    }

    private static void DesNestedGroupItem(GroupItem item, ref RecCursor cursor, CbRecord target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            target[item.Name] = DesGroupItem(item, ref cursor);
        }
        else
        {
            var values = new CbRecord[occurs];

            for (int i = 0; i < occurs; i++)
            {
                values[i] = DesGroupItem(item, ref cursor);
            }

            target[item.Name] = values;
        }
    }

    private static void DesElementaryDataItem(ElementaryDataItem item, ref RecCursor cursor, CbRecord target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            var raw = cursor.Read(item.Pic.StorageOccupied);

            // 是 FILLER，直接跳過
            if (item.IsFiller != true)
            {
                target[item.Name] = PicClauseCodec.ForMeta(item.Pic).Decode(raw);
            }
        }
        else
        {
            var values = new object?[occurs];

            for (int i = 0; i < occurs; i++)
            {
                var raw = cursor.Read(item.Pic.StorageOccupied);

                // 是 FILLER，直接跳過
                if (item.IsFiller != true)
                {
                    target[item.Name] = PicClauseCodec.ForMeta(item.Pic).Decode(raw);
                }
            }

            // 如果不是 FILLER，才建立陣列在 target
            if (item.IsFiller != true)
            {
                target[item.Name] = values;
            }
        }
    }
}