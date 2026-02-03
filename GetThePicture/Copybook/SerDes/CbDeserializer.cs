using GetThePicture.Copybook.Compiler.Ir;
using GetThePicture.Copybook.Compiler.Ir.Base;
using GetThePicture.Copybook.SerDes.Field;
using GetThePicture.Copybook.SerDes.Record;
using GetThePicture.PictureClause;

namespace GetThePicture.Copybook.SerDes;

internal class CbDeserializer
{
    internal static CbRecord DesSchema(CbSchema schema, ref CbFieldAccessor accessor)
    {
        if (schema.StorageOccupied != accessor.Size)
            throw new InvalidOperationException($"Record size mismatch: schema={schema.StorageOccupied}, actual={accessor.Size}");
        
        var result = ReadGroupItems(schema, ref accessor);

        return result;
    }

    private static CbRecord ReadGroupItems(IDataItem item, ref CbFieldAccessor accessor)
    {
        var result = new CbRecord();

        foreach (var child in item.Children)
        {
            switch (child)
            {
                case GroupItem g:
                    ReadNestedGroupItem(g, ref accessor, result);
                    break;

                case ElementaryDataItem e :
                    ReadElementaryDataItem(e, ref accessor, result);
                    break;

                default:
                    throw new InvalidOperationException($"Unsupported subordinate type: {child.GetType().Name}");
            }
        }

        return result;
    }

    private static void ReadNestedGroupItem(GroupItem item, ref CbFieldAccessor accessor, CbRecord target)
    {
        int occurs = item.Occurs ?? 1;

        if (occurs == 1)
        {
            target[item.Name] = ReadGroupItems(item, ref accessor);
        }
        else
        {
            var values = new CbRecord[occurs];

            for (int i = 0; i < occurs; i++)
            {
                values[i] = ReadGroupItems(item, ref accessor);
            }

            target[item.Name] = values;
        }
    }

    private static void ReadElementaryDataItem(ElementaryDataItem item, ref CbFieldAccessor accessor, CbRecord target)
    {
        int occurs = item.Occurs ?? 1;

        if (item.IsFiller) return;

        int elementSize = item.Pic.StorageOccupied;

        if (elementSize != item.StorageOccupied)
        {
            throw new InvalidOperationException(
                $"ElementaryDataItem storage size calculation error. " +
                $"Name={item.Name}, " +
                $"Pic={item.Pic}, " +
                $"ElementSize={elementSize}, " +
                $"Occurs={occurs}, " +
                $"Expected={elementSize * occurs}, " +
                $"Actual={item.StorageOccupied}");
        }

        if (occurs == 1)
        {
            var raw = accessor.Read(item.Offset, item.StorageOccupied);

            target[item.Name] = PicClauseCodec.ForMeta(item.Pic).Decode(raw);
        }
        else
        {            
            var values = new object?[occurs];

            for (int i = 0; i < occurs; i++)
            {
                int shift = item.StorageOccupied * i;
                var raw = accessor.Read(item.Offset + shift, item.StorageOccupied);

                values[i] = PicClauseCodec.ForMeta(item.Pic).Decode(raw);
            }

            target[item.Name] = values;
        }
    }
}