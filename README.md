# Get The Picture
> 讀懂你COBOL的明白  

針對`COBOL PICTURE`子句解析出現代語言程式可映射出來的資料種類。  

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

# 資料型態對照表 (W.I.P.)

## 字串
| COBOL PIC   | 說明                   |  對應  |  支援  | 說明 |
| ----------- | ---------------------- | :---------: | :----: | :--: |
| `PIC X(n)`  | 任意字元，長度 n            | `string`  | ✅ | -- |
| `PIC A(n)`  | 只允許字母                 | `string`  | ✅ | -- |
| `PIC AN(n)` | 字母 + 數字                | `string`  | ❌ | 請用 `PIC X(n)` |
| `PIC G(n)`  | 雙位元組字元 (DBCS, EBCDIC) | `string`  | ❌ | 請用 `PIC X(n)` |

<br>

## 整數

| COBOL PIC                   | 位數 (n) | SIGNED 對應 (範圍)                                                           | UNSIGNED 對應 (範圍)                                           |  支援  |
| :-------------------------- | :-----: | :--------------------------------------------------------------------------- | :------------------------------------------------------------- | :----: |
| `PIC 9(1)` \~ `PIC 9(2)`<br>`PIC S9(1)` \~ `PIC S9(2)`     | 1–2 位   | `sbyte`<br>範圍 **-128 \~ 127**                                              | `byte`<br>範圍 **0 \~ 255**                         | -- |
| `PIC 9(3)` \~ `PIC 9(4)`<br>`PIC S9(3)` \~ `PIC S9(4)`     | 3–4 位   | `short`<br>範圍 **-32,768 \~ 32,767**                                        | `ushort`<br>範圍 **0 \~ 65,535**                    | -- |
| `PIC 9(5)` \~ `PIC 9(9)`<br>`PIC S9(5)` \~ `PIC S9(9)`     | 5–9 位   | `int`<br>範圍 **-2,147,483,648 \~ 2,147,483,647**                            | `uint`<br>範圍 **0 \~ 4,294,967,295**               | -- |
| `PIC 9(10)` \~ `PIC 9(18)`<br>`PIC S9(10)` \~ `PIC S9(18)` | 10–18 位 | `long`<br>範圍 **-9,223,372,036,854,775,808 \~ 9,223,372,036,854,775,807**   | `ulong`<br>範圍 **0 \~ 18,446,744,073,709,551,615** | ✅ |
| `PIC 9(19)` \~ `PIC 9(28)`<br>`PIC S9(19)` \~ `PIC S9(28)` | 19-28 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28**                           | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28**  | -- |

不支援超過`28`位的整數位數  

<br>

## 小數

| COBOL PIC        |  位數 (n+m) |  說明                         |       對應         |  支援  |
| ---------------- |  :-------:  | :--------------------------- | :----------------- | :----: |
| `PIC 9(n)V9(m)`  |   1–28 位   | 無號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ |
| `PIC S9(n)V9(m)` |   1–28 位   | 有號小數，整數 n 位，小數 m 位 | `decimal`<br>範圍 **±1.0x10^-28 \~ ±7.9228x10^28** | ✅ |

不支援超過`28`位的精度位數組合  

<br>

## 時間/日期

| COBOL PIC                    |   用途   |  對應  |  支援  |
| ---------------------------- | ------- | :-------: | :----: |
| `PIC X(8)` (YYYYMMDD)        | 日期     | `DateOnly` | -- |
| `PIC X(6)` (HHmmss)          | 時間     | `TimeOnly` | -- |
| `PIC X(9)` (HHmmssSSS)       | 時間     | `TimeOnly` | -- |
| `PIC X(14)` (YYYYMMDDHHmmss) | 時間戳記 | `DateTime` | -- |

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
