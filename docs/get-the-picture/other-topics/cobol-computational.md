# COMP-3 (`Packed Decimal`)
與 `DISPLAY` 不同，`COMP-3` 不是用**字元**儲存數字，而是用 `Binary Packed` 的方式壓縮存放。

<br>

## 基本儲存規則
- 每個 byte = `2 個` nibble（`4 bits`）
- 每個 nibble 儲存 一個十進位數字
- **最後一個** nibble 為 `sign（符號）`

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

> ⚠️ 原則上（IBM、ACUCOBOL、GnuCOBOL） `C / D / F` 是最常見且相容性最高的組合，目前不實做切換功能。

<br>

## 位元組長度計算
> bytes = `ceil(nibbles / 2)`  

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

PIC S9(5)V9(2) COMP-3 (Decimal Digits : 2)  
| 值          | Packed Hex    |
| ---------- | ------------- |
| `12345.67` | `12 34 56 7C` |
| `-123.45`  | `00 12 34 5D`  |

<br><br>


# COMP-4 (`Binary`) / COMP-5 (`Native binary`)
在 COBOL 中，`COMP-4` 屬於二進位整數（binary integer）儲存格式。於大型主機環境（例如 IBM z/OS COBOL）中，其資料以 **Big Endian** 方式儲存。  
因此，在以 **Little Endian** 為主的現代平台（如 x86 / x64）上進行解析或寫入時，必須進行位元組順序轉換（byte order reversal），以確保與大型主機資料格式相容。  

與 `DISPLAY` 或 `COMP-3` 不同，COMPUTATIONAL 類型使用 **二補數（two’s complement）** 表示整數數值，不包含任何字元或十進位壓縮格式。  
`COMP-4` 在大多數系統上代表**明確使用二進位整數格式儲存**。其存在的歷史原因是：
- 在某些舊系統中 `COMP` 的實作並非完全一致
- 為避免歧義，使用 `COMP-4` 來明確指定 binary integer

<br>

## 📖 COMP 是什麼？
`COMP`（Computational）是早期 COBOL 的泛稱。在不同編譯器中可能代表：
- 二進位整數（binary）
- 或依機器最佳化的 native 格式。  

<br>

## 📖 BINARY 是什麼？
`BINARY` 是 ANSI/ISO COBOL 標準中較明確的寫法，在現代系統上通常與 `COMP` 等效。

<br>

## COMP-4 vs COMP-5

| USAGE  | 行為                                            | Endian    |
| ------ | ---------------------------------------------- | ---------- |
| COMP-4 | 可能受 PIC 長度限制                              | Big Endian |
| COMP-5 | 不受 PIC 範圍限制，精度由底層 `Binary Length` 決定 | (Platform-dependent) |

<br>

|  Symbols  | COMP-4 Range |   COMP-5 Range  |
| :-------- | ------------ | --------------- |
|  PIC 9    |     0 ~ 9    |      0 ~ 65535  |
|  PIC S99  |   -99 ~ +99  | -32768 ~ +32767 |
|  PIC 999  |     0 ~ 999  |      0 ~ 65535  |

> ⚠️ 目前沒有對 `BINARY` / `COMP` / `COMP-4` 做範圍限制，底層都是呼叫 `COMP-5`  

<br>

## 編譯器通則

| Digits (`n`) | Binary Length | C# 對應型別 (Signed) | C# 對應型別 (Unsigned) | 實際儲存 |
| ------------ | ------------- | ---------------- | ------------------ | :---------: |
| 1 – 4        | 2 bytes       | `short`          | `ushort`           |  16-bit  |
| 5 – 9        | 4 bytes       | `int`            | `uint`             |  32-bit  |
| 10 – 18      | 8 bytes       | `long`           | `ulong`            |  64-bit  |

> ⚠️ 實際分配結果取決於 編譯器實作，但以上對應為 IBM / Micro Focus / GnuCOBOL 的通用行為。  

<br>

## Storage Occupied

```cobol
01 WS-A PIC 9(4) COMP-4.
01 WS-B PIC 9(5) COMP-4.
01 WS-C PIC 9(10) COMP-4.
```

| Item | PIC          | Digits | Storage | 說明      |
| ---- | ------------ | :----: | ------- | ------- |
| WS-A | 9(4) COMP-4  | 4      | 2 bytes | `short` |
| WS-B | 9(5) COMP-4  | 5      | 4 bytes | `int`   |
| WS-C | 9(10) COMP-4 | 10     | 8 bytes | `long`  |

<br>

## Signed vs Unsigned

```cobol
PIC S9(4) COMP-4.   *> signed
PIC  9(4) COMP-4.   *> unsigned
```

| PIC          | Signed | C# Decode 型別            |
| ------------ | ------ | ----------------------- |
| `S9(n) COMP` | Yes    | `short / int / long`    |
| `9(n) COMP`  | No     | `ushort / uint / ulong` |

### 補數行為說明
- 所有 signed COMP 數值皆使用 two’s complement
- C# BitConverter.GetBytes(short/int/long) 與 COBOL 行為一致
- 不需額外處理 sign bit

<br><br>


# COMP-6 (`Unsigned Packed Decimal`)
`COMP-6` 並非 ANSI/ISO 標準 COBOL 定義，而是多數商用 COBOL（如 IBM Enterprise COBOL、Micro Focus）提供的擴充型態。  

| 特性 | COMP-3 | COMP-6 |
|------|--------|--------|
| 類型 | Packed Decimal | Unsigned Packed Decimal |
| 有 sign nibble | ✅ 有 | ❌ 無 |
| 可負數 | ✅ | ❌ |
| 標準支援 | ANSI/ISO | Vendor Extension |

<br>

```cobol
01  WS-COMP3-FIELD       PIC 9(5) COMP-3.
01  WS-COMP6-FIELD       PIC 9(6) COMP-6.
 
*> COMP-3: 5 digits F sign in 3 bytes (12345F)
*> COMP-6: 6 digits in 3 bytes (123456)
```

<br><br>

# 參考

Rocket Software ACUCOBOL-GT extend (V10.5.0) : [USAGE Clause](https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html)  
IBM COBOL for Linux on x86 (1.2.0) : [Computational items](https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=clause-computational-items)  
IBM Enterprise COBOL for z/OS (6.5.0) : [TRUNC](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=options-trunc)

<br><br>