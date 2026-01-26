using System.Text.Json;

using GetThePicture.Cobol.Picture;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Copybook.Obsolete.Writer;

public class JsonWriter
{
    public void Write(Utf8JsonWriter writer, IDataItem root)
    {
        ArgumentNullException.ThrowIfNull(writer);
        ArgumentNullException.ThrowIfNull(root);

        WriteItem(writer, root);
    }

    private void WriteItem(Utf8JsonWriter writer, IDataItem item)
    {
        switch (item)
        {
            case CbSchema document:
                WriteDocument(writer, document);
                break;

            case GroupItem group:
                WriteGroup(writer, group);
                break;

            case ElementaryDataItem elementary:
                WriteElementary(writer, elementary);
                break;

            default:
                throw new NotSupportedException(
                    $"Unsupported IDataItem type: {item.GetType().Name}");
        }
    }

    private void WriteDocument(Utf8JsonWriter writer, CbSchema document)
    {
        writer.WriteStartObject();

        writer.WriteString("Type", "Document");

        writer.WritePropertyName("DataItem");
        writer.WriteStartArray();
        foreach (var DataItem in document.DataItems)
            WriteItem(writer, DataItem);
        writer.WriteEndArray();

        writer.WriteEndObject();
    }

    private void WriteGroup(Utf8JsonWriter writer, GroupItem group)
    {
        writer.WriteStartObject();

        writer.WriteString("Type", "GroupItem");
        writer.WriteNumber("Level", group.Level);
        writer.WriteString("Name", group.Name);
        if (group.Comment is not null)
            writer.WriteString("Comment", group.Comment);
        if (group.Occurs.HasValue)
            writer.WriteNumber("Occurs", group.Occurs.Value);
        writer.WritePropertyName("Subordinate");
        writer.WriteStartArray();
        foreach (var subordinate in group.Subordinates)
            WriteItem(writer, subordinate);
        writer.WriteEndArray();

        writer.WriteEndObject();
    }

    private static void WriteElementary(Utf8JsonWriter writer, ElementaryDataItem item)
    {
        writer.WriteStartObject();

        writer.WriteString("Type", "ElementaryDataItem");
        writer.WriteNumber("Level", item.Level);
        writer.WriteString("Name", item.Name);
        if (item.Comment is not null)
            writer.WriteString("Comment", item.Comment);
        if (item.Pic is not null)
        {
            writer.WritePropertyName("Pic");
            WritePic(writer, item.Pic);
        }
        if (item.Occurs.HasValue)
            writer.WriteNumber("Occurs", item.Occurs.Value);
        if (item.Value is not null)
            WriteValue(writer, item.Value);

        writer.WriteEndObject();
    }

    private static void WritePic(Utf8JsonWriter writer, PicMeta pic)
    {
        writer.WriteStartObject();

        writer.WriteString("Class", pic.BaseClass.ToString());
        writer.WriteString("Semantic", pic.Semantic.ToString());
        writer.WriteString("Usage", pic.Usage.ToString());

        writer.WritePropertyName("Info");
        WritePicInfo(writer, pic);

        writer.WriteEndObject();
    }

    private static void WritePicInfo(Utf8JsonWriter writer, PicMeta pic)
    {
        writer.WriteStartObject();
        writer.WriteBoolean("Signed", pic.Signed);
        writer.WriteNumber("DigitCount", pic.DigitCount);
        writer.WriteNumber("StorageOccupied", pic.StorageOccupied);
        writer.WriteEndObject();
    }

    private static void WriteValue(Utf8JsonWriter writer, object value)
    {
        writer.WritePropertyName("Value");

        switch (value)
        {
            case string s:
                writer.WriteStringValue(s);
                break;

            case int i:
                writer.WriteNumberValue(i);
                break;

            case decimal d:
                writer.WriteNumberValue(d);
                break;

            default:
                writer.WriteStringValue(value.ToString());
                break;
        }
    }
}