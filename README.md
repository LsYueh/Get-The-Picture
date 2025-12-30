# Get The Picture
> 讀懂你COBOL的明白  

針對`COBOL PICTURE`子句解析出現代語言程式可映射出來的資料種類。  

<br><br>

# COBOL PICTURE (PIC) 子句

目前支援的解析格式  

基本:
- PIC X
- PIC X(4)
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
| COBOL PIC   | 說明                   |  對應  |
| ----------- | ---------------------- | :---------: |
| `PIC X(n)`  | 任意字元，長度 n            | `String`  |
| `PIC A(n)`  | 只允許字母                 | `String`  |
| `PIC AN(n)` | 字母 + 數字                | `String`  |
| `PIC G(n)`  | 雙位元組字元 (DBCS, EBCDIC) | `String`  |

<br>

## 整數/小數

| COBOL PIC        | 說明                 |  對應  |
| ---------------- | -------------------- | :--------: |
| `PIC 9(n)`       | 無號小數，整數 n 位     |  `Number`  |
| `PIC S9(n)`      | 有號小數，整數 n 位     |  `Number`  |
| `PIC 9(n)V9(m)`  | 無號小數，整數 n 位，小數 m 位 |  `Number`  |
| `PIC S9(n)V9(m)` | 有號小數，整數 n 位，小數 m 位 |  `Number`  |

<br>

## 時間/日期

| COBOL PIC                    |   用途   |  對應  |
| ---------------------------- | ------- | :-------: |
| `PIC X(8)` (YYYYMMDD)        | 日期     | `Date`   |
| `PIC X(6)` (HHmmss)          | 時間     | `Date`   |
| `PIC X(9)` (HHmmssSSS)       | 時間     | `Date`   |
| `PIC X(14)` (YYYYMMDDHHmmss) | 時間戳記 | `Date`   |

<br><br>

# 正負號數字`S9`字串轉換  (W.I.P.)
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

|  選 項  | 對 應 COBOL |
| :----: | :---- |
| ca | RM/COBOL (not RM/COBOL-85) |
| cb | MBP COBOL |
| ci | IBM COBOL (RM/COBOL-85) |
| cm | Micro Focus COBOL |
| cn | NCR COBOL |
| cr | Realia COBOL |
| cv | VAX COBOL |

<br><br>
