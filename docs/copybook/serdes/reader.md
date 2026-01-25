# Reader

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
using GetThePicture.Codec.Utils;
using GetThePicture.Copybook;
using GetThePicture.Copybook.Compiler.Ir;
```

```csharp
Encoding cp950 = EncodingFactory.CP950;
using var streamReader = new StreamReader(@"TestData/demo.cpy", cp950);

Document document = Reader.FromStreamReader(streamReader);

// Debug / dump
document.Dump(Console.Out);
```
呼叫 `FromStreamReader()` 後會產出中繼資料，可搭配 `Writer` 做其他格式輸出。  

<br>

Dump 輸出內容:  
```shell
COPYBOOK
  1 CUSTOMER-RECORD
    5 CUSTOMER-ID >> PIC: Class='Numeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
    5 CUSTOMER-NAME >> PIC: Class='Alphanumeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display'
    5 ACCOUNT-BALANCE >> PIC: Class='Numeric' (Semantic='None'), Signed=True, Int=5, Dec=2, Len=7, Usage='PackedDecimal'
```

<br>
