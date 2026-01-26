# Copybook Compiler

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

範例程式:  
```csharp
using GetThePicture.Copybook.Compiler;    // CbCompiler
using GetThePicture.Copybook.Compiler.Ir; // CbSchema

using GetThePicture.PictureClause.Utils;  // EncodingFactory
```

```csharp
Encoding cp950 = EncodingFactory.CP950;
using var streamReader = new StreamReader(@"TestData/demo.cpy", cp950);

CbSchema schema = CbCompiler.FromStreamReader(streamReader);

schema.Dump(Console.Out);
```
呼叫 `CbCompiler.FromStreamReader()` 後會產出中繼資料，可搭配 `Dump()` 在開發中進行除錯。  

<br>

Dump 輸出內容:  
```shell
COPYBOOK-SCHEMA
  1 CUSTOMER-RECORD
    5 CUSTOMER-ID >> PIC: Class='Numeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
    5 CUSTOMER-NAME >> PIC: Class='Alphanumeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display'
    5 ACCOUNT-BALANCE >> PIC: Class='Numeric' (Semantic='None'), Signed=True, Int=5, Dec=2, Len=7, Usage='PackedDecimal'
```

<br>
