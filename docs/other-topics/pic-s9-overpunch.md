# `S9`數字轉換規則
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
