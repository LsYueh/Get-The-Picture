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

# COBOL Coding Sheet (Reference Format)
COBOL 程式有一套固定的欄位規則，尤其在 `固定格式（Fixed Format）` 下很重要。主要分為 `Sequence Area`, `Indicator Area`, `Area A`, `Area B` 等區域。

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

# Elementary Item

在 COBOL 中，`Elementary Item`（基本項目）是 Data Division 中 Data Description Entry 的最基本單位。它通常是不能再被分解的欄位，也就是最小的資料單位，通常會直接對應到記憶體中的一段連續空間。  

特性:  
1. 不可分割：它不能再由其他子欄位構成（不像 Group Item 可以包含其他欄位）。  
2. 有 PIC 描述：Elementary Item 通常會有 PIC（Picture）子句，定義它的類型和長度，例如數字、字母或字元。  
3. 記憶體映射：每個 Elementary Item 對應到一段實際記憶體，可用來存放資料。  
4. 可使用 VALUE 初始化：可以設定初始值。  

<br>

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID       PIC 9(5).         *> Elementary item, 整數 5 位
   05 CUSTOMER-NAME     PIC X(20).        *> Elementary item, 字元 20 位
   05 CUSTOMER-BALANCE  PIC S9(7)V99.     *> Elementary item, 浮點數 (小數 2 位)
```

- `CUSTOMER-ID`、`CUSTOMER-NAME`、`CUSTOMER-BALANCE` 都是 Elementary Items。  
- `CUSTOMER-RECORD` 是 Group Item，因為它包含多個 Elementary Items。

<br>

## 記憶體對應

```python
CUSTOMER-RECORD (Group Item)
┌─────────────┬───────────────────────┬──────────────────┐
│ CUSTOMER-ID │ CUSTOMER-NAME         │ CUSTOMER-BALANCE │
│ 5 bytes     │ 20 bytes              │ 9 bytes          │  <- Memory Layout
└─────────────┴───────────────────────┴──────────────────┘
```

- 每個 Elementary Item 對應記憶體連續區塊  
- Group Item 只是 容器，本身不直接存資料  

<br><br>

# Usage 子句

`USAGE` 定義欄位在記憶體中的儲存方式，影響資料的物理編碼與運算行為。  
- DISPLAY（預設）：以可讀字元存放，每個數字或字母對應一個 byte，便於輸入輸出與檢視。DISPLAY numeric 可能包含 Overpunch 符號。  
- COMP / COMP-5（Binary）：以二進位形式存放，運算效率高，但不可直接讀取文字。  
- COMP-3（Packed Decimal）：將兩個數字壓縮在一個 nibble，最後一個 nibble 用於符號，節省空間且方便算術運算。  

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

<br><br>

## 類別(`Category`)資料

• [文字 (`Alphabetic`/`Alphanumeric`)](docs/cobol-picture/category/alphabetic-alphanumeric.md)  
• [數字 (`Numeric`)](docs/cobol-picture/category/numeric.md)  

<br><br>

## 語意(`Semantic`)資料

• [日期 (`Date`)](docs/cobol-picture/semantic/date-time/date.md)  
• [時間 (`Time`)](docs/cobol-picture/semantic/date-time/time.md)  
• [時間戳記 (`Timestamp`)](docs/cobol-picture/semantic/date-time/timestamp.md)  

<br><br>

# COBOL Copybook
`Copybook` 是 COBOL 中用來定義資料結構的重用檔案，透過 COPY 指令引入，常用於描述檔案格式、資料欄位配置與記憶體布局。在大型主機與金融系統中，Copybook 是資料交換與系統整合的核心。  

Copybook 通常包含：
- 欄位階層（Level Number）
- 資料型別與長度（PIC 子句）
- 儲存格式（如 DISPLAY、COMP、COMP-3）

由於 Copybook 直接對應到位元與位元組配置，它不僅是程式碼的一部分，更是系統間共用的資料規格說明書。  

```cobol
       * Sample COBOL Copybook
       * Defines a fixed-length record layout
       * Total length: 23 bytes

       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID       PIC 9(8).             *> Numeric ID
           05  CUSTOMER-NAME     PIC X(10).            *> Text
           05  ACCOUNT-BALANCE   PIC S9(5)V99  COMP-3. *> Packed decimal
```

<br><br>

# 其他說明

• [`S9`數字轉換規則](docs/other-topics/pic-s9-overpunch.md)  
• [`COMPUTATIONAL` 轉換規則](docs/other-topics/cobol-computational.md)  

<br><br>

# 參考

Rocket Software ACUCOBOL-GT extend (V10.5.0) : [USAGE Clause](https://docs.rocketsoftware.com/zh-TW/bundle/acucobolgt_dg_1050_html/page/BKRFRFDATAS043.html)  
IBM Enterprise COBOL for z/OS (6.5.0) : [USAGE clause](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=entry-usage-clause)  
IBM COBOL for Linux on x86 (1.2.0) : [Classes and categories of data](https://www.ibm.com/docs/en/cobol-linux-x86/1.2.0?topic=relationships-classes-categories-data)  

<br><br>
