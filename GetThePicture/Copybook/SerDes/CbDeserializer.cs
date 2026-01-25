using GetThePicture.Codec;
using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.SerDes.Record;

namespace GetThePicture.Copybook.SerDes
;

internal class CbDeserializer
{
    internal static RecValue DesDocument(Document schema, ref RecCursor cursor)
    {
        var result = new RecValue();

        foreach (var dataItem in schema.DataItems)
        {
            switch (dataItem)
            {
                case GroupItem g:
                    DesNestedGroupItem(g, ref cursor, result);
                    break;

                case ElementaryDataItem e :
                    DesElementaryDataItem(e, ref cursor, result);
                    break;
                    
                default:
                    throw new InvalidOperationException($"Unsupported data item type: {dataItem.GetType().Name}");
            };
        }

        return result;
    }

    private static RecValue DesGroupItem(GroupItem item, ref RecCursor cursor)
    {
        var result = new RecValue();

        foreach (var subordinate in item.Subordinates)
        {
            switch (subordinate)
            {
                case GroupItem g:
                    DesNestedGroupItem(g, ref cursor, result);
                    break;

                case ElementaryDataItem e :
                    DesElementaryDataItem(e, ref cursor, result);
                    break;

                default:
                    throw new InvalidOperationException($"Unsupported subordinate type: {subordinate.GetType().Name}");
            }
        }

        return result;
    }

    private static void DesNestedGroupItem(GroupItem item, ref RecCursor cursor, RecValue target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            target[item.Name] = DesGroupItem(item, ref cursor);
        }
        else
        {
            var values = new RecValue[occurs];

            for (int i = 0; i < occurs; i++)
            {
                values[i] = DesGroupItem(item, ref cursor);
            }

            target[item.Name] = values;
        }
    }

    private static void DesElementaryDataItem(ElementaryDataItem item, ref RecCursor cursor, RecValue target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            var raw = cursor.Read(item.Pic.StorageOccupied);

            // 是 FILLER，直接跳過
            if (item.IsFiller != true)
            {
                target[item.Name] = CodecBuilder.ForPic(item.Pic).Decode(raw);
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
                    target[item.Name] = CodecBuilder.ForPic(item.Pic).Decode(raw);
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