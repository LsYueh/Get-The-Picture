# Copybook Resolver
Copybook Resolver 將 `CbLayout` 拓展成實際記憶體的起始與結束位置映資料物件 (`CbStorage`)。

## 流程概覽
```
CbLayout 物件結構
   │
   ▼
CbResolver
   │
   ├─ 走訪 GroupItem / ElementaryDataItem / Redefines 節點
   │
   ├─ 計算 Offset
   │
   └─ 建立 CbStorage 物件結構

```

<br>

## 使用方式

`demo.cpy`
```cobol
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID        PIC 9(5).
           05 CUSTOMER-NAME      PIC X(30).
           05 CUSTOMER-BALANCE   PIC S9(7)V99 COMP-3.
```

<br>

程式:
```csharp
using var streamReader = new StreamReader(@"TestData/demo.cpy", cp950);

CbLayout layout = CbCompiler.FromStreamReader(streamReader);

CbStorage storage = CbResolver.FromLayout(layout);

storage.Dump(Console.Out);
```

<br>

Dump 輸出內容:  
```shell
COPYBOOK-STORAGE-MAP
  CUSTOMER-RECORD start=1
    CUSTOMER-ID start=1 len=5 end=6
    CUSTOMER-NAME start=6 len=30 end=36
    CUSTOMER-BALANCE start=36 len=5 end=41
```

<br>

索引值採 `1-based` 方式顯示，以貼近 COBOL 在顯示資料時的習慣。
其中 `CUSTOMER-BALANCE` 的 USAGE 是 `COMP-3`，根據計算公式：

> bytes = (digits + 1) / 2  

所以 `S9(7)V99` 對應的資料長度為 (9 + 1) / 2 = `5`，Dump 顯示出的 `len=5` 實為記憶體的資料長度。

<br>
