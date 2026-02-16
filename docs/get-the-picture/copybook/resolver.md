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

> bytes = `ceil(nibbles / 2)`  

所以 `S9(7)V99` 對應的資料長度為 INTEGER (`9` + `1 Sign`) / 2 = `5`，Dump 顯示出的 `len=5` 實為記憶體的資料長度。

<br><br>

以下是具備巢狀與 OCCURS 子句的 Copybook

`nested-occurs-record.cpy`
```cobol
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  ORDER-RECORD.
           05  ORDER-ID              PIC X(10).
           05  CUSTOMER-NAME         PIC X(20).

           05  ORDER-LINES           OCCURS 3 TIMES.
               10  PRODUCT-CODE      PIC X(8).
               10  QUANTITY          PIC 9(3).
               10  LINE-AMOUNTS      OCCURS 2 TIMES.
                   15  AMOUNT        PIC 9(5)V99.

           05  TOTAL-AMOUNT          PIC 9(7)V99.
```

<br>

Layout Dump : 
```shell
COPYBOOK-LAYOUT
  1 ORDER-RECORD
    5 ORDER-ID >> PIC: [X(10)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display'
    5 CUSTOMER-NAME >> PIC: [X(20)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=20, Dec=0, Len=20, Usage='Display'
    5 ORDER-LINES OCCURS 3
      10 PRODUCT-CODE >> PIC: [X(8)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
      10 QUANTITY >> PIC: [9(3)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
      10 LINE-AMOUNTS OCCURS 2
        15 AMOUNT >> PIC: [9(5)V99] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=2, Len=7, Usage='Display'
    5 TOTAL-AMOUNT >> PIC: [9(7)V99] Class='Numeric' (Semantic='None'), Signed=False, Int=7, Dec=2, Len=9, Usage='Display'
```

<br>

Storage Dump :  
```shell
COPYBOOK-STORAGE-MAP
  ORDER-RECORD start=1
    ORDER-ID start=1 len=10 end=11
    CUSTOMER-NAME start=11 len=20 end=31
    ORDER-LINES(1) start=31
      PRODUCT-CODE start=31 len=8 end=39
      QUANTITY start=39 len=3 end=42
      LINE-AMOUNTS(1) start=42
        AMOUNT start=42 len=7 end=49
      LINE-AMOUNTS(2) start=49
        AMOUNT start=49 len=7 end=56
    ORDER-LINES(2) start=56
      PRODUCT-CODE start=56 len=8 end=64
      QUANTITY start=64 len=3 end=67
      LINE-AMOUNTS(1) start=67
        AMOUNT start=67 len=7 end=74
      LINE-AMOUNTS(2) start=74
        AMOUNT start=74 len=7 end=81
    ORDER-LINES(3) start=81
      PRODUCT-CODE start=81 len=8 end=89
      QUANTITY start=89 len=3 end=92
      LINE-AMOUNTS(1) start=92
        AMOUNT start=92 len=7 end=99
      LINE-AMOUNTS(2) start=99
        AMOUNT start=99 len=7 end=106
    TOTAL-AMOUNT start=106 len=9 end=115
```
