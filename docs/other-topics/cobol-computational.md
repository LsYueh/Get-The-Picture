# `COMP` (Binary) / `COMP-5` (Native Binary)
COMP（Binary / Computational）是 COBOL 中最早、也是效能最佳的數值儲存格式之一。  
與 DISPLAY 或 COMP-3 不同，COMP 使用 CPU 原生整數（二補數, two’s complement） 表示數值，不包含任何字元或 decimal encoding。  

<br>

規則（主流 COBOL 編譯器通則）
| Digits (`n`) | Binary Length | C# 對應型別 (Signed) | C# 對應型別 (Unsigned) |
| ------------ | ------------- | ---------------- | ------------------ |
| 1 – 4        | 2 bytes       | `short`          | `ushort`           |
| 5 – 9        | 4 bytes       | `int`            | `uint`             |
| 10 – 18      | 8 bytes       | `long`           | `ulong`            |

> ⚠️實際分配結果取決於 編譯器實作，但以上對應為 IBM / Micro Focus / GnuCOBOL 的通用行為。  

<br>

## Storage Occupied

```cobol
01 WS-A PIC 9(4) COMP.
01 WS-B PIC 9(5) COMP.
01 WS-C PIC 9(10) COMP.
```

| Item | PIC        | Digits | Storage | 說明      |
| ---- | ---------- | :----: | ------- | ------- |
| WS-A | 9(4) COMP  | 4      | 2 bytes | `short` |
| WS-B | 9(5) COMP  | 5      | 4 bytes | `int`   |
| WS-C | 9(10) COMP | 10     | 8 bytes | `long`  |

<br>

## Signed vs Unsigned

```cobol
PIC S9(4) COMP.   *> signed
PIC  9(4) COMP.   *> unsigned
```

| PIC          | Signed | C# Decode 型別            |
| ------------ | ------ | ----------------------- |
| `S9(n) COMP` | Yes    | `short / int / long`    |
| `9(n) COMP`  | No     | `ushort / uint / ulong` |

### 補數行為說明
- 所有 signed COMP 數值皆使用 two’s complement
- C# BitConverter.GetBytes(short/int/long) 與 COBOL 行為一致
- 不需額外處理 sign bit

<br>

## COMP vs COMP-5

| Picture Clause | COMP Range | COMP-5 Range |
| :------------- | ---------- | ------------ |
|     PIC 9      |    0 ~ 9   |      0 ~ 65535  |
|     PIC S99    |  -99 ~ +99 | -32768 ~ +32767 |
|     PIC 999    |    0 ~ 999 |      0 ~ 65535  |


<br><br>

# `COMP-3` (Packed Decimal)
與 DISPLAY 不同，COMP-3 不是用字元儲存數字，而是用 `Binary Packed` 的方式壓縮存放。

<br>

## 基本儲存規則
- 每個 byte = 2 個 nibble（4 bits）
- 每個 nibble 儲存 一個十進位數字
- 最後一個 nibble 為 sign（符號）

<br>

```r
| digit | digit | digit | sign |
```

<br>

## Sign Nibble 規則
|  Sign  | Trailing byte | |
| ---- | :--: | :--: |
|-Dca `Positive` | x'0F' | -- |
|-Dcb/-Dci/-Dcm/-Dcr `Positive` | x'0C' | ✅ |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Negative` | x'0D' | ✅ |
|-Dca/-Dcb/-Dci/-Dcm/-Dcr `Unsigned` | x'0F' | ✅ |
|-Dcv `Unsigned` | x'0C' | -- |

> 原則上（IBM、ACUCOBOL、GnuCOBOL）， `C / D / F` 是最常見且相容性最高的組合，目前不實做切換功能。

<br>

## 位元組長度計算
> bytes = (digits + 1) / 2  

| PIC                     | digits | bytes |
| ----------------------- | ------ | ----- |
| `PIC 9(3) COMP-3`       | 3      | 2     |
| `PIC S9(5) COMP-3`      | 5      | 3     |
| `PIC S9(5)V9(2) COMP-3` | 7      | 4     |

<br><br>

## 範例對照

PIC S9(5) COMP-3  
| 值        | Packed Hex |
| -------- | ---------- |
| `12345`  | `12 34 5C` |
| `-12345` | `12 34 5D` |

<br>

PIC 9(5) COMP-3（Unsigned）  
| 值       | Packed Hex |
| ------- | ---------- |
| `12345` | `12 34 5F` |

<br>

PIC S9(5)V9(2) COMP-3  
| 值          | Packed Hex    |
| ---------- | ------------- |
| `12345.67` | `12 34 56 7C` |
| `-123.45`  | `01 23 45 D`  |

<br><br>

# 參考

Rocket Software ACUCOBOL-GT extend (V10.5.0) : [USAGE Clause](https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html)  
IBM COBOL for Linux on x86 (1.2.0) : [Computational items](https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=clause-computational-items)  
IBM Enterprise COBOL for z/OS (6.5.0) : [TRUNC](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=options-trunc)

<br><br>