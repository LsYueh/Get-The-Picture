# Get The Picture
> 讀懂你COBOL的明白  

<br>

## 專案目的
> 透過簡單的文字 `X` 與數字 `9 / S9`，我們建構出長達百年的金融體系。  

<br>

COBOL 的 `PICTURE` 子句，以極少的符號，精確地描述出資料的**型態、長度、符號位、顯示格式與儲存語意**。
這套設計方式歷經數十年的實務驗證，支撐了銀行、保險、政府與大型企業的核心系統，至今仍在持續運作。

<br>

然而，在現代語言（例如C#、Java、TypeScript、Rust）中，這些語意往往被**隱含、分散或遺失**：

* `string` 與 `number` 無法完整表達 **定長、補零、符號位置、顯示與儲存差異**
* 解析邏輯常以 ad-hoc 的 `TryParse`、正則或硬編碼規則存在
* PIC 與現代型別之間缺乏**可驗證、可測試、可組合**的轉換模型

<br>

本專案的目的，是將 `COBOL PICTURE` 子句視為一種 **明確的資料規格（Data Specification）**，並：

### 將 PIC 語意轉換為可映射的現代資料模型

* 明確區分 **顯示格式（DISPLAY）** 與 **實際數值語意**
* 將 `9 / S9 / V / X / A` 等元素拆解為結構化資訊
* 建立可對應至現代語言型別（`int / long / decimal / string` 等）的判斷依據

### 建立可組合、可擴充的 Decode / Encode 流程

* 以 **Fluent / Builder 風格**描述解析上下文
* 將「字串 → 型別」與「型別 → 字串」視為對等的一階公民
* 讓轉換過程可被單元測試、驗證與重構

### 降低 COBOL 與現代系統整合的心智與實作成本

* 避免重複撰寫易出錯的解析邏輯
* 提供一致、可預期的行為邊界（精度、符號）
* 作為資料轉換、系統汰換、或雙軌運行的一部分

### 保留歷史系統的「語意」，而不只是資料

本專案不試圖「現代化」COBOL語言，而是**尊重並保存其資料設計哲學**，使其能被現代語言理解、驗證與安全地使用。

<br>

## 適用情境

* 核心系統資料轉出
* COBOL 與現代服務的資料交換層
* 舊系統重構或漸進式汰換
* 對 PIC 規格進行靜態分析或測試驗證

<br><br>

# COBOL PICTURE (PIC) 子句

目前支援的解析格式  

基本:
- PIC X
- PIC X(4)
- PIC A
- PIC A(4)
- PIC 9999
- PIC 9(4)

混合:
- PIC 99V99
- PIC 9(3)V9(2)
- PIC 9(3)V99
- PIC S99V99
- PIC S9(3)V9(2)
- PIC S9(3)V99

<br><br>

# 使用方式
```csharp
using GetThePicture.Cobol.Picture;
using GetThePicture.Codec;
```

<br>

## 字串
基本使用:  
```csharp
var pic = Pic.Parse("X(5)");

// Encode: CLR → COBOL PICTURE
CobolValueCodec.ForPic(pic).Encode("AbC"); // >> "AbC  "

// Decode: COBOL PICTURE → CLR
CobolValueCodec.ForPic(pic).Decode("ABC  "); // >> "ABC"
```

<br>

中文字(`CP950`)處理:  
```csharp
var pic = Pic.Parse("X(7)");

CobolValueCodec.ForPic(pic).Decode("中文字 "); // >> "中文字"

CobolValueCodec.ForPic(pic).Encode("中文字"); // >> "中文字 "
```

```csharp
var pic = Pic.Parse("X(5)");

// 宣告長度不夠
CobolValueCodec.ForPic(pic).Encode("中文字"); // >> "中文?"
```

<br>

## 浮點數:  
```csharp
var pic = Pic.Parse("S9(3)V9");

// Encode: CLR → COBOL PICTURE
CobolValueCodec.ForPic(pic).Encode( 12.3); // >> "012C"
CobolValueCodec.ForPic(pic).Encode(-12.3); // >> "012L"

// Encode: CLR → COBOL PICTURE (ACUCOBOL)
CobolValueCodec.ForPic(pic).WithDataStorageOption(DataStorageOptions.CA).Decode(12.3); // >> "0123"

// Decode: COBOL PICTURE → CLR
CobolValueCodec.ForPic(pic).Decode("12L"); // >> -12.3
```


<br><br>

# 基本資料型態對照表

## 字串
| COBOL PIC   | 說明                   |  對應  |  編碼  | 解碼  | 說明 |
| ----------- | ---------------------- | :---------: | :----: |:----: | :--: |
| `PIC X(n)`  | 任意字元，長度 n            | `string`  | ✅ |✅ | -- |
| `PIC A(n)`  | 只允許字母                 | `string`  | ✅ |✅ | -- |
| `PIC AN(n)` | 字母 + 數字                | `string`  | ❌ |❌ | 請用 `PIC X(n)` |
| `PIC G(n)`  | 雙位元組字元 (DBCS, EBCDIC) | `string`  | ❌ |❌ | 請用 `PIC X(n)` |

<br>

## 整數

| COBOL PIC                   | 位數 (n) | SIGNED 對應 (範圍)                                                           | UNSIGNED 對應 (範圍)                                           |  編碼  |  解碼  |
| :-------------------------- | :-----: | :--------------------------------------------------------------------------- | :------------------------------------------------------------- | :----: | :----: |
| `PIC 9(1)` \~ `PIC 9(2)`<br>`PIC S9(1)` \~ `PIC S9(2)`     | 1–2 位   | `sbyte`<br>範圍 **-128 \~ 127**                                              | `byte`<br>範圍 **0 \~ 255**                         | ✅ | ✅ |
| `PIC 9(3)` \~ `PIC 9(4)`<br>`PIC S9(3)` \~ `PIC S9(4)`     | 3–4 位   | `short`<br>範圍 **-32,768 \~ 32,767**                                        | `ushort`<br>範圍 **0 \~ 65,535**                    | ✅ | ✅ |
| `PIC 9(5)` \~ `PIC 9(9)`<br>`PIC S9(5)` \~ `PIC S9(9)`     | 5–9 位   | `int`<br>範圍 **-2,147,483,648 \~ 2,147,483,647**                            | `uint`<br>範圍 **0 \~ 4,294,967,295**               | ✅ | ✅ |
| `PIC 9(10)` \~ `PIC 9(18)`<br>`PIC S9(10)` \~ `PIC S9(18)` | 10–18 位 | `long`<br>範圍 **-9,223,372,036,854,775,808 \~ 9,223,372,036,854,775,807**   | `ulong`<br>範圍 **0 \~ 18,446,744,073,709,551,615** | ✅ | ✅ |
| `PIC 9(19)` \~ `PIC 9(28)`<br>`PIC S9(19)` \~ `PIC S9(28)` | 19-28 位 | `decimal (scale = 0)`<br>範圍 **約 ±7.9228x10^28**                           | `decimal (scale = 0)`<br>範圍 **約 ±7.9228x10^28**  | ✅ | ✅ |

不支援超過`28`位的整數位數  

<br>

## 小數

| COBOL PIC        |  位數 (n+m) |  說明                         |       對應         |  編碼  |  解碼  |
| ---------------- |  :-------:  | :--------------------------- | :----------------- | :----: | :----: |
| `PIC 9(n)V9(m)`  |   1–28 位   | 無號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ | ✅ |
| `PIC S9(n)V9(m)` |   1–28 位   | 有號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ | ✅ |

不支援超過`28`位的精度位數組合  

<br><br>

# 特殊資料型態對照表

## 日期

| COBOL PIC                    |  用途  |   對應   |  編碼  |  解碼  | 說明 |
| ---------------------------- | ------ | :-----: | :----: | :----: | :--: |
| `PIC X(8)` (YYYYMMDD)        |  日期  | `DateOnly` | ✅ | ✅ | 西元年 |
| `PIC X(7)` (yyyMMDD)         |  日期  | `DateOnly` | ✅ | ✅ | 民國年 |

<br>

## 時間

| COBOL PIC                    |   用途   |  對應  |  編碼  |  解碼  |
| ---------------------------- | ------- | :-------: | :----: | :----: |
| `PIC X(6)` (HHmmss)          | 時間     | `TimeOnly` | -- | -- |
| `PIC X(9)` (HHmmssSSS)       | 時間     | `TimeOnly` | -- | -- |

<br>

## 時間戳記

| COBOL PIC                    |   用途   |  對應  |  編碼  |  解碼  |
| ---------------------------- | ------- | :-------: | :----: | :----: |
| `PIC X(14)` (YYYYMMDDHHmmss) | 時間戳記 | `DateTime` | -- | -- |

> 其他戳記格式可透過 `PIC X(7)` (yyyMMDD) + `PIC X(6)` (HHmmss) = `PIC X(13)` (yyyMMDDHHmmss) 的方式進行組合

<br><br>

# 正負號數字`S9`字串轉換
`Signed overpunch`源自Hollerith(赫爾曼·何樂禮)打孔卡編碼，為解決正負號帶來的字元浪費與孔位長度變動的問題，將`數字`與`正負號`整併至一個孔位解讀。目前已知COBOL的對應關係有：

|  數 字  | ca,<br>cb,<br>cm,<br>cr<br>Positive | ci,<br>cn<br>Positive | ca,<br>ci,<br>cn<br>Negative | cb<br>Negative | cm<br>Negative | cr<br>Negative |
| :---- | :---- | :------ | :------ | :------ | :------ | :------ |
| 0 | '0' | '{' | '}' | '@' | 'p' | ' ' (space) |
| 1 | '1' | 'A' | 'J' | 'A' | 'q' | '!' |
| 2 | '2' | 'B' | 'K' | 'B' | 'r' | '"' (double-quote) |
| 3 | '3' | 'C' | 'L' | 'C' | 's' | '#' |
| 4 | '4' | 'D' | 'M' | 'D' | 't' | '$' |
| 5 | '5' | 'E' | 'N' | 'E' | 'u' | '%' |
| 6 | '6' | 'F' | 'O' | 'F' | 'v' | '&' |
| 7 | '7' | 'G' | 'P' | 'G' | 'w' | ''' (single-quote) |
| 8 | '8' | 'H' | 'Q' | 'H' | 'x' | '(' |
| 9 | '9' | 'I' | 'R' | 'I' | 'y' | ')' |

<br>

|  選 項  | 對 應 COBOL | 說明 |
| :----: | :---- | :--: |
| ca | RM/COBOL (not RM/COBOL-85) | -- |
| cb | MBP COBOL | -- |
| ci | IBM COBOL (RM/COBOL-85) | -- |
| cm | Micro Focus COBOL | -- |
| cn | NCR COBOL | -- |
| cr | Realia COBOL | -- |
| cv | VAX COBOL | 找不到 Overpunch Codex |

<br>

COBOL的`S9`對正負號數字表示範例 (IBM COBOL)
```cobol
PIC S9(3) VALUE -123.
TRAILING                 '1'   '2'   'L'
TRAILING SEPARATE  '1'   '2'   '3'   '-'
LEADING                  'J'   '2'   '3'
LEADING SEPARATE   '-'   '1'   '2'   '3'

PIC S9(5)V9 VALUE -12345.6.
TRAILING                '1'  '2'  '3'  '4'  '5'  'O'
TRAILING SEPARATE  '1'  '2'  '3'  '4'  '5'  '6'  '-'
LEADING                 'J'  '2'  '3'  '4'  '5'  '6'
LEADING SEPARATE   '-'  '1'  '2'  '3'  '4'  '5'  '6'
```

<br>

```js
// IBM COBOL (-Dci) and Trailing

'S9(3)V9': '12C' >> 12.3
'S9(3)V9': '12L' >> -12.3

'S9(1)V9': '12C' >> 2.3
'S9(1)V9': '12L' >> -2.3

```

<br>

雖然支援`Leading`解析，但是會跟某些`Overpunch`查表定義衝突

```csharp
// IBM COBOL (-Dci) and Leading

'S9(3)V9': 'J23' >> -12.3

'S9(1)V9': 'J23' >> `Exception: Unknown overpunch char: '2'`

```

```csharp
// Micro Focus COBOL (-Dcm) and Leading

'S9(1)V9': 'J23' >> 2.3

```

<br><br>

# COMP-3 (planning)

|  Sign  | Trailing byte |
| ---- | :--: |
|-Dca `Positive` | x'0F' |
|-Dcb/-Dci/-Dcm/-Dcr `Positive` | x'0C' |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Negative` | x'0D' |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Unsigned` | x'0F' |
|-Dcv `Unsigned` | x'0C' |

<br>

## Difference between COMP and COMP-3

|  COMP  | COMP-3 |
| :----: | :----: |
| It represents the data in pure binary form. | It represents the data in packed decimal form. |
| Can use only `9` and `S` in PIC Clause. | Ccan use `9` , `S` , `V` in PIC Clause. |
| COMP usage stores the data in `half word` or in `full word`, depending on the size of the data. | COMP3 usage stores `1 digit` in `half byte (i.e. 4 bits)` and a separate `1 bit` is reserved for the sign, which is stored at the right side of the data. |
| The memory to be occupied by the data according to the length is predefined i.e. : <br> • 9(01) - 9(04) : 16 bits (2 bytes) <br> • 9(05) - 9(09) :  32 bits (4 bytes) <br> • S9(10) - S9(18) :  64 bits (8 bytes) | The memory to be occupied by the data is defined by the following formula: <br> • (length of variable + 1)/2 bytes. <br> <br> Example : The memory occupied by S9(3) is: <br> (3+1)/2 i.e. 2 bytes. |
| COMP does not occupy extra space to store sign. | In COMP3 sign in compulsorily stored at right side and thus it occupies an extra space. |

<br><br>
