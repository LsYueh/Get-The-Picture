# Copybook Writer - JSON

<br>

使用`Reader`建立`IR`
```csharp
var doc = Reader.FromStreamReader(new StreamReader(@"TestData/t30-tse.cpy", cp950));
```

<br>

使用.Net內建的JSON Writer
```csharp
using var stream = new MemoryStream();
using var writer = new Utf8JsonWriter(stream, new JsonWriterOptions {
    Indented = true,
    Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping
});
```

<br>

使用本函示庫提供的 `JsonWriter`
```csharp
var jsonWriter = new JsonWriter();

jsonWriter.Write(writer, doc);
writer.Flush();

string json = Encoding.UTF8.GetString(stream.ToArray());

Console.WriteLine(json);
```

<br>

輸出內容:
```json
{
  "Type": "Document",
  "DataItem": [
    {
      "Type": "ElementaryDataItem",
      "Level": 1,
      "Name": "STOCK-NO",
      "Comment": "股票代號",
      "Pic": {
        "Class": "Alphanumeric",
        "Semantic": "None",
        "Usage": "Display",
        "Info": {
          "Signed": false,
          "DigitCount": 6,
          "StorageOccupied": 6
        }
      }
    },
    {
      "Type": "ElementaryDataItem",
      "Level": 1,
      "Name": "BULL-PRICE",
      "Comment": "漲停價",
      "Pic": {
        "Class": "Numeric",
        "Semantic": "None",
        "Usage": "Display",
        "Info": {
          "Signed": false,
          "DigitCount": 9,
          "StorageOccupied": 9
        }
      }
    },
    (以下省略...)
  ]
}
```

<br>
