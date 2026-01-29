# Elementary Data Item

在 COBOL 中，`Elementary Data Item`（基本項目）是 Data Division 中 Data Description Entry 的最基本單位。它通常是不能再被分解的欄位，也就是最小的資料單位，通常會直接對應到記憶體中的一段連續空間。  

特性:  
1. 不可分割：它不能再由其他子欄位構成（不像 Group Item 可以包含其他欄位）。  
2. 有 PIC 描述：Elementary Item 通常會有 PIC（Picture）子句，定義它的類型和長度，例如數字、字母或字元。  
3. 記憶體映射：每個 Elementary Item 對應到一段實際記憶體，可用來存放資料。  
4. 可使用 VALUE 初始化：可以設定初始值。  

<br>

```cobol
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID       PIC 9(5).         *> Elementary Data item, 整數 5 位
   05 CUSTOMER-NAME     PIC X(20).        *> Elementary Data item, 字元 20 位
   05 CUSTOMER-BALANCE  PIC S9(7)V99.     *> Elementary Data item, 浮點數 (小數 2 位)
```

- `CUSTOMER-ID`、`CUSTOMER-NAME`、`CUSTOMER-BALANCE` 都是 Elementary Data Items。  
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
