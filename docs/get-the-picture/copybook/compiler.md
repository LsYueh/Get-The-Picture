# Copybook Compiler
把 Copybook 原始文字直接編譯成**物件結構** (`CbLayout`)，並將每個 COBOL 元素轉換為可閱讀的資料描述。

## 流程概覽
```
.cpy 檔案
   │
   ▼
CbCompiler
   │
   ├─ Lexer
   │    └─ 將原始文字拆解成 Token
   │       (Level / Name / PIC / OCCURS / REDEFINES 等)
   │
   ├─ Parser
   │    └─ 把 Token 解析成資料層級與父子關係
   │
   └─ 建立 CbLayout 物件結構
        ├─ GroupItem
        ├─ ElementaryDataItem
        ├─ Level 66 / Level 88
        └─ etc...
```

<br>

## 使用方式

`demo.cpy`
```cobol
      * Sample COBOL Copybook
      * Defines a fixed-length record layout
      * Total length: 23 bytes
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID       PIC 9(8).             *> Numeric ID
           05  CUSTOMER-NAME     PIC X(10).            *> Text
           05  ACCOUNT-BALANCE   PIC S9(5)V99  COMP-3. *> Packed decimal
```

<br>

程式:  
```csharp
using var streamReader = new StreamReader(@"TestData/demo.cpy", cp950);

CbLayout layout = CbCompiler.FromStreamReader(streamReader);

layout.Dump(Console.Out);
```
呼叫 `CbCompiler.FromStreamReader()` 後會產出中繼資料，可搭配 `Dump()` 在開發中進行除錯。  

<br>

Dump 輸出內容:  
```shell
COPYBOOK-LAYOUT
  1 CUSTOMER-RECORD
    5 CUSTOMER-ID >> PIC: Class='Numeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
    5 CUSTOMER-NAME >> PIC: Class='Alphanumeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display'
    5 ACCOUNT-BALANCE >> PIC: Class='Numeric' (Semantic='None'), Signed=True, Int=5, Dec=2, Len=7, Usage='PackedDecimal'
```

<br>
