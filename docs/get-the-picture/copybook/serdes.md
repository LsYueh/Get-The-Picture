# Copybook SerDes (⚠️Obsolete)
SerDes 是 `Serialization`（序列化）與 `Deserialization`（反序列化）的合稱，用於資料在不同系統或存儲之間的轉換。  

<br>

1. Deserialization（反序列化）  
    將序列化後的資料恢復成程式中的 `物件` 或 `資料結構` (目前採用Dictionary)。 

    ![work flow](/docs/get-the-picture/deserialize-work-flow.png)  

    ```csharp
    // 提供 Copybook 的 layout 與 storage
    var provider = new DataProvider(new StreamReader(@"TestData/t30-otc.cpy", cp950));

    // 建立 Serializer/Deserializer
    var serDes = new CbSerDes(provider);

    // 讀取檔案 (編碼: CP950 / ASCII)
    using var reader = new StreamReader(@"TestData/t30-otc-lite.dat", cp950);

    string? line;
    while ((line = reader.ReadLine()) != null)
    {
        var byte = cp950.GetBytes(line);

        // 根據Copybook的資料格式來反序列化 (Deserialize) 資料
        CbRecord record = serDes.Deserialize(expected);

        Console.WriteLine("==== Record ====");
        record.Print();
        Console.WriteLine("================\n");
    }
    ```

    <details>
        <summary>輸出結果：</summary>

    ```shell
    ...
    ==== Record ====
    STOCK-NO: 19094
    BULL-PRICE: 105.80000
    LDC-PRICE: 96.20000
    BEAR-PRICE: 86.60000
    LAST-MTH-DATE: 20251111
    SETTYPE: 0
    MARK-W: 0
    MARK-P: 0
    MARK-L: 0
    IND-CODE: 00
    IND-SUB-CODE: 
    MARK-M: 0
    STOCK-NAME: 榮成四
    MARK-W-DETAILS:
    MATCH-INTERVAL: 0
    ORDER-LIMIT: 0
    ORDERS-LIMIT: 0
    PREPAY-RATE: 0
    MARK-S: 0
    STK-MARK: 0
    MARK-F: 0
    MARK-DAY-TRADE: 
    STK-CTGCD: 0
    ================
    ...
    ```
        
    </details>

    <br>

    > ⚠️ 目前不支援包含 `Level 66`、`Level 77`、`REDEFINES` 子句的反序列化處裡  

<br>

2. Serialization（序列化）  
    將程式中的物件或資料結構轉換成一種 `可存儲` 或 `傳輸` 的格式。  

    ![work flow](/docs/get-the-picture/serialize-work-flow.png)  

    ```csharp
    var serialized = serDes.Serialize(record);
    ```

<br>

**SerDes** 的相關使用範例位於 [CbSerDesTest.cs](/GetThePicture.Tests/Copybook/SerDes/CbSerDesTest.cs) 內有標記 `[TestCategory("Demo")]` 的測試項目中。

<br>

## Level 66 (`RENAMES … THRU`) in Copybook
目前僅解析並保留位於 record 末端的 Level 66 `RENAMES … THRU` 定義。此類 Level 66 不影響實體儲存結構，僅表達既有欄位的語意聚合，適合作為語意資訊保存。常用於描述**類似 RECORD KEY 的邏輯識別範圍（logical key）**。  

此限制確保被 `RENAMES` 涵蓋的欄位範圍為線性、連續且可預期，並降低解析複雜度。同時為未來進行語意型態轉換或資料庫 DDL 投影預留擴充空間。

<br>

📖 更多關於 [RECORD KEY Clause](/docs/get-the-picture/copybook/record-key-clause.md) ...  

<br><br>

# Performance

## 數據內容
- 根據**櫃買中心** (OTC) 規格改寫的 `T30.CPY` (包含註解)：DataItem 24 個   
- 部分**櫃買中心** (OTC) 的 `T30.DAT`：漲跌幅度資料 55 筆   

<br>

## 跑分結果
Ver. `26.10.3`

```bash
BenchmarkDotNet v0.15.8, Windows 11 (10.0.26200.7623/25H2/2025Update/HudsonValley2)
Intel Core i5-10400 CPU 2.90GHz, 1 CPU, 12 logical and 6 physical cores
.NET SDK 8.0.417
  [Host]     : .NET 8.0.23 (8.0.23, 8.0.2325.60607), X64 RyuJIT x86-64-v3
  DefaultJob : .NET 8.0.23 (8.0.23, 8.0.2325.60607), X64 RyuJIT x86-64-v3
```

| Method                | Mean       | Error     | StdDev    |
|---------------------- |-----------:|----------:|----------:|
| Deserialize           | 173.918 μs | 0.7304 μs | 0.6832 μs |
| Serialize             | 193.912 μs | 0.9445 μs | 0.8835 μs |
| Deserialize_Serialize | 384.769 μs | 3.8755 μs | 3.4355 μs |

<br>

> ⚠️ T30 的資料內沒有進行 `COMP`，目前的跑分算是 Best Case。  

<br><br>
