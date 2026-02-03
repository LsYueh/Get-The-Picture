# Get The Picture
Modern .NET library for working with COBOL Copybook–based data  
用於處理以 COBOL Copybook 為基礎資料的現代 .NET 類別庫  

> **讀懂你 COBOL 的明白**  

## 開發需求
- **.NET 8.0** 或更新版本
- **C# 12** 或相容版本（.NET 8 預設）

## 輸入格式需求
- COBOL Copybook (`.cpy`) 純文字檔案
- ASCII / CP950 編碼

<br><br>

# 專案目的
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

<br>

## 輔助工具  
  - [Copycat](docs/copycat/README.md) - 將 `Copybook` 內容轉換成可在 `C#` 使用的強型別資料模型(Sealed Class)。

<br><br>

# COBOL Copybook
`Copybook` 是 COBOL 中用來定義資料結構的重用檔案，透過 COPY 指令引入，常用於描述檔案格式、資料欄位配置與記憶體布局。在大型主機與金融系統中，Copybook 是資料交換與系統整合的核心。  

Copybook 通常包含：
- 欄位階層（Level Number）
- 資料型別與長度（PIC 子句）
- 儲存格式（如 DISPLAY、COMP、COMP-3）

由於 Copybook 直接對應到位元與位元組配置，它不僅是程式碼的一部分，更是系統間共用的資料規格說明書。  

<br>

## Copybook SerDes
SerDes 是 `Serialization`（序列化）與 `Deserialization`（反序列化）的合稱，用於資料在不同系統或存儲之間的轉換。  

<br>

1. Deserialization（反序列化）
    - 將序列化後的資料恢復成程式中的 `物件` 或 `資料結構` (目前採用Dictionary)。 

    ```csharp
    // 將 Copybook 轉成 layout
    var layout = CbCompiler.FromStreamReader(new StreamReader(@"TestData/t30-otc.cpy", cp950));
    var serDes = new CbSerDes(layout);

    // 讀取檔案 (編碼: CP950 / ASCII)
    using var reader = new StreamReader(@"TestData/t30-otc-lite.dat", cp950);

    string? line;
    while ((line = reader.ReadLine()) != null)
    {
        var byte = cp950.GetBytes(line);

        // 根據Copybook的layout來反序列化資料
        CbRecord record = serDes.Deserialize(expected);

        Console.WriteLine("==== Record ====");
        record.Print();
        Console.WriteLine("================\n");
    }
    ```

    輸出: 
    ```shell
    ...
    ==== Record ====
    STOCK-NO: 19094
    BULL-PRICE: 105.80000
    LDC-PRICE: 96.20000
    BEAR-PRICE: 86.60000
    LAST-MTH-DATE: 20251111
    SETTYPE: 0
    MARK-W: 0
    MARK-P: 0
    MARK-L: 0
    IND-CODE: 00
    IND-SUB-CODE: 
    MARK-M: 0
    STOCK-NAME: 榮成四
    MARK-W-DETAILS:
      MATCH-INTERVAL: 0
      ORDER-LIMIT: 0
      ORDERS-LIMIT: 0
      PREPAY-RATE: 0
    MARK-S: 0
    STK-MARK: 0
    MARK-F: 0
    MARK-DAY-TRADE: 
    STK-CTGCD: 0
    ================
    ...
    ```

    > ⚠️ 目前不支援包含 `Level 66`、`Level 77`、`REDEFINES` 子句的反序列化處裡  

<br>

2. Serialization（序列化）
    - 將程式中的物件或資料結構轉換成一種 `可存儲` 或 `傳輸` 的格式。

    ```csharp
    var serialized = serDes.Serialize(record);
    ```

<br>

**SerDes** 的相關使用範例位於 [CbSerDesTest.cs](GetThePicture.Tests\Copybook\SerDes\CbSerDesTest.cs) 內有標記 `[TestCategory("Demo")]` 的測試項目中。

<br>

- 更多關於 [Copybook Compiler](docs/get-the-picture/copybook/compiler.md) ...

<br>

## Level 66 (`RENAMES … THRU`) in Copybook
目前僅解析並保留位於 record 末端的 Level 66 `RENAMES … THRU` 定義。此類 Level 66 不影響實體儲存結構，僅表達既有欄位的語意聚合，適合作為語意資訊保存。常用於描述**類似 RECORD KEY 的邏輯識別範圍（logical key）**。  

此限制確保被 `RENAMES` 涵蓋的欄位範圍為線性、連續且可預期，並降低解析複雜度。同時為未來進行語意型態轉換或資料庫 DDL 投影預留擴充空間。

<br>

- 更多關於 [`RECORD KEY` Clause](docs/get-the-picture/copybook/record-key-clause.md) ... 

<br><br>

# COBOL Coding Sheet (Reference Format)
COBOL 程式有一套固定的欄位規則，尤其在 `固定格式（Fixed Format）` 下很重要。主要分為 `Sequence Area`, `Indicator Area`, `Area A`, `Area B` 等區域。

<br>

```cobol
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01 ORDER-RECORD.
           05 ORDER-ID           PIC 9(6).
           05 ORDER-DATE         PIC 9(8).
           05 ORDER-AMOUNT       PIC S9(7)V99 COMP-3.
```

<br>

| 位置 (Column) | 說明                                                                 |
| ----------- | ------------------------------------------------------------------ |
| 1–6         | **Sequence Number**（序號欄，可選）：用於列印或版本控制。                             |
| 7           | **Indicator Area**（指示欄）：<br> - `*`：註解<br> - `/`：換頁<br> - `-`：延續上一行 |
| 8–11        | **Area A**：段落名稱、Section 名稱、DIVISION 關鍵字等。                          |
| 12–72       | **Area B**：語句、指令、變數宣告、程式碼本體。                                       |
| 73–80       | **Identification Area**（識別欄，可選）：通常用於序號或其他控制用途。                     |

> 現代 COBOL `(Free Format) ` 已經不限制欄位，但固定格式仍常用於舊系統。  

<br><br>

# COBOL Level Numbers 基本概念

COBOL 使用 `Level Number`（層級號） 來描述資料結構，主要有：

| Level         | 用途             | 說明                  |
| ------------- | -------------- | ------------------- |
| **01**        | 主結構            | 定義檔案或記錄的頂層結構        |
| **05/10/15…** | 子結構            | 01 之下的子群組或欄位，形成巢狀結構 |
| **66**        | RENAMES        | 將已有欄位重新命名或形成別名區段    |
| **77**        | 單一變數           | 不屬於群組，獨立使用          |
| **88**        | Condition Name | 定義邏輯條件（True/False）  |

> ⚠️ Level number 越小層級越高，01 是最外層。

## 詳細說明
- Level [66 — RENAMES](docs/get-the-picture/cobol-level-numbers/lv66.md)
- Level [77 — Standalone Variable (單一變數)](docs/get-the-picture/cobol-level-numbers/lv77.md)
- Level [88 — Condition Name](docs/get-the-picture/cobol-level-numbers/lv88.md)  

<br><br>

# `Elementary Data Item` and `Group Item` 

| 面向                    | Elementary Data Item    | Group Item             |
| --------------------- | ----------------------- | ---------------------- |
| 定義角色                  | **最小資料單位（leaf）**        | **結構性容器（composite）**   |
| 是否可包含子項目              | ❌ 不可                    | ✅ 可                    |
| 是否有 `PIC` 子句          | ✅ **必須有**               | ❌ **不可有**              |
| 是否直接描述資料型態            | ✅ 是（數值、字元、COMP、COMP-3…） | ❌ 否（由子項目間接決定）          |
| 是否可直接被 MOVE / COMPUTE | ✅ 可                     | ⚠️ 可（視情況，為整段記憶體移動）     |
| 記憶體佔用                 | 由 `PIC` 決定              | 為所有子項目記憶體的總和           |
| 可否有 `OCCURS`          | ✅ 可                     | ✅ 可                    |
| 可否有 `REDEFINES`       | ✅ 可                     | ✅ 可                    |
| 可否有 `VALUE`           | ✅ 可                     | ❌（標準上 group 不定義 VALUE） |
| 是否為樹的葉節點              | ✅ 是                     | ❌ 否                    |
| COBOL 規格名稱            | *Elementary data item*  | *Group item*           |

<br>

• [Elementary Data Item](docs/get-the-picture/cobol/ElementaryDataItem.md)  

<br><br>

# Usage 子句

`USAGE` 定義欄位在記憶體中的儲存方式，影響資料的物理編碼與運算行為。  
- DISPLAY（預設）：以可讀字元存放，每個數字或字母對應一個 byte，便於輸入輸出與檢視。DISPLAY numeric 可能包含 Overpunch 符號。  
- COMP / COMP-5（Binary）：以二進位形式存放，運算效率高，但不可直接讀取文字。  
- COMP-3（Packed Decimal）：將兩個數字壓縮在一個 nibble，最後一個 nibble 用於符號，節省空間且方便算術運算。  
  - [`COMPUTATIONAL` 轉換規則](docs/get-the-picture/other-topics/cobol-computational.md)  

<br>

| Class | Category/Semantic | Usage |
| :---: | :---------------: | ----- |
| Alphabetic | Alphabetic | DISPLAY |
| Alphanumeric | Alphanumeric | DISPLAY |
| Date-Time <br> (Alphanumeric) | Date <br> Time <Timestamp> | DISPLAY |
| Numeric | Numeric | DISPLAY <br> COMP (Binary) <br> COMP-3 (Packed Decimal) <br> COMP-5 (Native Binary) |

<br><br>

# PICTURE (PIC) 子句

支援PIC語法  

| Alphabetic | Alphanumeric | Numeric | Numeric (With Sign) |
| :--------: | :----------: | :-----: | :-----------------: |
| PIC A.. <br> PIC A(n) | PIC X.. <br> PIC X(n) | PIC 9... <br> PIC 9(n) <br> PIC 9...V9... <br> PIC 9(n)V9(m) <br> PIC 9(n)V9... | PIC S9... <br> PIC S9(n) <br> PIC S9...V9... <br> PIC S9(n)V9(m) <br> PIC S9(n)V9... |

<br>

## 類別(`Category`)資料

- [文字 (`Alphabetic`/`Alphanumeric`)](docs/get-the-picture/cobol-picture/category/alphabetic-alphanumeric.md)  
- [數字 (`Numeric`)](docs/get-the-picture/cobol-picture/category/numeric.md)  
  - [`S9`數字轉換規則](docs/get-the-picture/other-topics/pic-s9-overpunch.md)  

<br>

## 語意(`Semantic`)資料

- [日期 (`Date`)](docs/get-the-picture/cobol-picture/semantic/date-time/date.md)  
- [時間 (`Time`)](docs/get-the-picture/cobol-picture/semantic/date-time/time.md)  
- [時間戳記 (`Timestamp`)](docs/get-the-picture/cobol-picture/semantic/date-time/timestamp.md)  

<br><br>

# RENAMES 子句

請參考 Level [66 — RENAMES](docs/get-the-picture/cobol-level-numbers/lv66.md)

<br><br>

# REDEFINES 子句

## 與 `66 RENAMES` 的差異
|            | RENAMES         | REDEFINES       |
| ---------- | --------------- | --------------- |
| 影響 storage | ❌               | ✅               |
| 改變 offset  | ❌               | ✅（對齊另一個）        |
| 本體是        | 邏輯群組            | **GroupItem**   |
| 最終表現       | View / Property | View / Property |

<br>

## 支援說明

在 IBM 提供的 [REDEFINES clause](https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=entry-redefines-clause) 文件中，整理出幾種 `REDEFINES` 可能的使用與法規則：

<br>

**CASE 1**：Group REDEFINES Elementary Data Item
```cobol
05  A PICTURE X(6).
05  B REDEFINES A.
    10 B-1          PICTURE X(2).
    10 B-2          PICTURE 9(4).
05  C               PICTURE 99V99.

```

<br>

**CASE 2**：01-level + GLOBAL
```cobol
01 A1 PICTURE X(6). 
01 B1 REDEFINES A1 GLOBAL PICTURE X(4). 
```

<br>

**CASE 3a**：多個 REDEFINES 指向同一 target
```cobol
05  A               PICTURE 9999.
05  B REDEFINES A   PICTURE 9V999.
05  C REDEFINES A   PICTURE 99V99.
```

**CASE 3b**：REDEFINES 鏈
```cobol
05  A               PICTURE 9999.
05  B REDEFINES A   PICTURE 9V999.
05  C REDEFINES B   PICTURE 99V99.
```

<br>

### 支援狀態總覽

| Case | 用法說明 | 支援狀態 | 說明 |
|------|----------|----------|------|
| CASE 1 | Group REDEFINES Elementary Data Item | ✅ 支援 | 最常見且結構單純的用法。Group 僅作為 Elementary Item 的另一種結構化視角，不引入額外 storage。 |
| CASE 2 | 01-level REDEFINES + GLOBAL | ❌ 不支援 | 涉及 01-level overlay 與 GLOBAL 可視範圍，在高階語言中難以安全對應。 |
| CASE 3a | 多個 REDEFINES 指向同一 target | ❌ 不支援 | 會形成多重 storage alias，容易造成資料覆寫與語意不明確。 |
| CASE 3b | REDEFINES 鏈（REDEFINES 已 REDEFINES 的 item） | ❌ 不支援 | 需解析並正規化多層 alias 關係，實作與維護成本過高。 |


<br><br>

# 參考

Rocket Software ACUCOBOL-GT extend (V10.5.0) : [USAGE Clause](https://docs.rocketsoftware.com/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html)  
IBM Enterprise COBOL for z/OS (6.5.0) : [USAGE clause](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=entry-usage-clause)  
IBM Enterprise COBOL for z/OS (6.5.0) : [RECORD KEY clause](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=section-record-key-clause)  
IBM COBOL for Linux on x86 (1.2.0) : [Classes and categories of data](https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=relationships-classes-categories-data)  

<br><br>
