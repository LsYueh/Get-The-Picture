using System.Text;

using GetThePicture.Codec;
using GetThePicture.Codec.Utils;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.SerDes
;

public class Deserializer
{
    private static readonly Encoding cp950 = EncodingFactory.CP950;

    internal static RecordValue DesDocument(Document schema, ref RecordCursor cursor)
    {
        var result = new RecordValue();

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

    private static RecordValue DesGroupItem(GroupItem item, ref RecordCursor cursor)
    {
        var result = new RecordValue();

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

    private static void DesNestedGroupItem(GroupItem item, ref RecordCursor cursor, RecordValue target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            target[item.Name] = DesGroupItem(item, ref cursor);
        }
        else
        {
            var values = new RecordValue[occurs];

            for (int i = 0; i < occurs; i++)
            {
                values[i] = DesGroupItem(item, ref cursor);
            }

            target[item.Name] = values;
        }
    }

    private static void DesElementaryDataItem(ElementaryDataItem item, ref RecordCursor cursor, RecordValue target)
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