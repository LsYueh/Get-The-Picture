# Forge
將 Copybook 中的 PICTURE（資料欄位結構）轉換為 `CbWrapper` 的子類別實作。  

Forge 會依據 Storage Tree 中 `Level 1` 節點的唯一性判斷，決定是否縮減生成的屬性名稱。  

若 Copybook 內僅存在單一個 Level 1 Group Item，則會自動以該節點名稱作為 Wrapper Class 名稱基底，並在屬性生成時**省略該層級前綴**，以提升程式碼可讀性。  

<br>

# 使用方式

使用 `--copybook` 指定要轉換的 Copybook  
```bash
forge --copybook Copybooks/t30-otc.cpy
```

輸出結果:  
```bash
New wrapper class generated: "D:\Projects\get-the-picture\GetThePicture.Forge\T30Otc.cs"
```

<br>

<details>
    <summary>產出的 Wrapper Class 內容：</summary>

```csharp
using GetThePicture.Copybook.Wrapper;
using GetThePicture.Copybook.Wrapper.Base;

namespace GetThePicture;

public class T30Otc_t(byte[] raw) : CbWrapper(raw)
{
    // ----------------------------
    // Copybook Address Map
    // ----------------------------

    protected override Dictionary<string, CbAddress> AddressMap { get; } = new Dictionary<string, CbAddress>
    {
        ["STOCK-NO"]                       = new CbAddress(   1,   6, "X(6)"), // 股票代號
        ["BULL-PRICE"]                     = new CbAddress(   7,   9, "9(5)V9(4)"), // 漲停價
        ["LDC-PRICE"]                      = new CbAddress(  16,   9, "9(5)V9(4)"), // 開盤競價基準
        ["BEAR-PRICE"]                     = new CbAddress(  25,   9, "9(5)V9(4)"), // 跌停價
        ["LAST-MTH-DATE"]                  = new CbAddress(  34,   8, "9(8)"), // 上次成交日
        ["SETTYPE"]                        = new CbAddress(  42,   1, "X(01)"), // 交易方式
        ["MARK-W"]                         = new CbAddress(  43,   1, "X(01)"), // 處置股票註記
        ["MARK-P"]                         = new CbAddress(  44,   1, "X(01)"), // 注意股票註記
        ["MARK-L"]                         = new CbAddress(  45,   1, "X(01)"), // 委託限制註記
        ["IND-CODE"]                       = new CbAddress(  46,   2, "X(02)"), // 產業別代碼
        ["IND-SUB-CODE"]                   = new CbAddress(  48,   2, "X(02)"), // 證券別代碼
        ["MARK-M"]                         = new CbAddress(  50,   1, "X(01)"), // 豁免平盤下融券賣出註記
        ["STOCK-NAME"]                     = new CbAddress(  51,  16, "X(16)"), // 股票中文名稱
        ["MARK-W-DETAILS::MATCH-INTERVAL"] = new CbAddress(  67,   3, "9(03)"), // 撮合循環時間（分）
        ["MARK-W-DETAILS::ORDER-LIMIT"]    = new CbAddress(  70,   6, "9(06)"), // 單筆委託限制數量（張）
        ["MARK-W-DETAILS::ORDERS-LIMIT"]   = new CbAddress(  76,   6, "9(06)"), // 多筆委託限制數量（張）
        ["MARK-W-DETAILS::PREPAY-RATE"]    = new CbAddress(  82,   3, "9(03)"), // 款券預收成數（%）
        ["MARK-S"]                         = new CbAddress(  85,   1, "X(01)"), // 豁免平盤下借券賣出註記
        ["STK-MARK"]                       = new CbAddress(  86,   1, "X(01)"), // 類股註記
        ["MARK-F"]                         = new CbAddress(  87,   1, "X(01)"), // 面額註記
        ["MARK-DAY-TRADE"]                 = new CbAddress(  88,   1, "X(01)"), // 可現股當沖註記
        ["STK-CTGCD"]                      = new CbAddress(  89,   1, "X(01)"), // 板別註記
        ["FILLER01"]                       = new CbAddress(  90,  11, "X(11)"),
    };

    // ----------------------------
    // Strongly Typed Properties
    // ----------------------------

    /// <summary>
    /// STOCK-NO X(6) : 股票代號
    /// </summary>
    public string StockNo
    {
        get => this["STOCK-NO"].Get<string>();
        set => this["STOCK-NO"].Set(value);
    }

    /// <summary>
    /// BULL-PRICE 9(5)V9(4) : 漲停價
    /// </summary>
    public decimal BullPrice
    {
        get => this["BULL-PRICE"].Get<decimal>();
        set => this["BULL-PRICE"].Set(value);
    }

    /// <summary>
    /// LDC-PRICE 9(5)V9(4) : 開盤競價基準
    /// </summary>
    public decimal LdcPrice
    {
        get => this["LDC-PRICE"].Get<decimal>();
        set => this["LDC-PRICE"].Set(value);
    }

    /// <summary>
    /// BEAR-PRICE 9(5)V9(4) : 跌停價
    /// </summary>
    public decimal BearPrice
    {
        get => this["BEAR-PRICE"].Get<decimal>();
        set => this["BEAR-PRICE"].Set(value);
    }

    /// <summary>
    /// LAST-MTH-DATE 9(8) : 上次成交日
    /// </summary>
    public uint LastMthDate
    {
        get => this["LAST-MTH-DATE"].Get<uint>();
        set => this["LAST-MTH-DATE"].Set(value);
    }

    /// <summary>
    /// SETTYPE X(01) : 交易方式
    /// </summary>
    public string Settype
    {
        get => this["SETTYPE"].Get<string>();
        set => this["SETTYPE"].Set(value);
    }

    /// <summary>
    /// MARK-W X(01) : 處置股票註記
    /// </summary>
    public string MarkW
    {
        get => this["MARK-W"].Get<string>();
        set => this["MARK-W"].Set(value);
    }

    /// <summary>
    /// MARK-P X(01) : 注意股票註記
    /// </summary>
    public string MarkP
    {
        get => this["MARK-P"].Get<string>();
        set => this["MARK-P"].Set(value);
    }

    /// <summary>
    /// MARK-L X(01) : 委託限制註記
    /// </summary>
    public string MarkL
    {
        get => this["MARK-L"].Get<string>();
        set => this["MARK-L"].Set(value);
    }

    /// <summary>
    /// IND-CODE X(02) : 產業別代碼
    /// </summary>
    public string IndCode
    {
        get => this["IND-CODE"].Get<string>();
        set => this["IND-CODE"].Set(value);
    }

    /// <summary>
    /// IND-SUB-CODE X(02) : 證券別代碼
    /// </summary>
    public string IndSubCode
    {
        get => this["IND-SUB-CODE"].Get<string>();
        set => this["IND-SUB-CODE"].Set(value);
    }

    /// <summary>
    /// MARK-M X(01) : 豁免平盤下融券賣出註記
    /// </summary>
    public string MarkM
    {
        get => this["MARK-M"].Get<string>();
        set => this["MARK-M"].Set(value);
    }

    /// <summary>
    /// STOCK-NAME X(16) : 股票中文名稱
    /// </summary>
    public string StockName
    {
        get => this["STOCK-NAME"].Get<string>();
        set => this["STOCK-NAME"].Set(value);
    }

    /// <summary>
    /// MATCH-INTERVAL 9(03) : 撮合循環時間（分）
    /// </summary>
    public ushort MarkWDetails_MatchInterval
    {
        get => this["MARK-W-DETAILS::MATCH-INTERVAL"].Get<ushort>();
        set => this["MARK-W-DETAILS::MATCH-INTERVAL"].Set(value);
    }

    /// <summary>
    /// ORDER-LIMIT 9(06) : 單筆委託限制數量（張）
    /// </summary>
    public uint MarkWDetails_OrderLimit
    {
        get => this["MARK-W-DETAILS::ORDER-LIMIT"].Get<uint>();
        set => this["MARK-W-DETAILS::ORDER-LIMIT"].Set(value);
    }

    /// <summary>
    /// ORDERS-LIMIT 9(06) : 多筆委託限制數量（張）
    /// </summary>
    public uint MarkWDetails_OrdersLimit
    {
        get => this["MARK-W-DETAILS::ORDERS-LIMIT"].Get<uint>();
        set => this["MARK-W-DETAILS::ORDERS-LIMIT"].Set(value);
    }

    /// <summary>
    /// PREPAY-RATE 9(03) : 款券預收成數（%）
    /// </summary>
    public ushort MarkWDetails_PrepayRate
    {
        get => this["MARK-W-DETAILS::PREPAY-RATE"].Get<ushort>();
        set => this["MARK-W-DETAILS::PREPAY-RATE"].Set(value);
    }

    /// <summary>
    /// MARK-S X(01) : 豁免平盤下借券賣出註記
    /// </summary>
    public string MarkS
    {
        get => this["MARK-S"].Get<string>();
        set => this["MARK-S"].Set(value);
    }

    /// <summary>
    /// STK-MARK X(01) : 類股註記
    /// </summary>
    public string StkMark
    {
        get => this["STK-MARK"].Get<string>();
        set => this["STK-MARK"].Set(value);
    }

    /// <summary>
    /// MARK-F X(01) : 面額註記
    /// </summary>
    public string MarkF
    {
        get => this["MARK-F"].Get<string>();
        set => this["MARK-F"].Set(value);
    }

    /// <summary>
    /// MARK-DAY-TRADE X(01) : 可現股當沖註記
    /// </summary>
    public string MarkDayTrade
    {
        get => this["MARK-DAY-TRADE"].Get<string>();
        set => this["MARK-DAY-TRADE"].Set(value);
    }

    /// <summary>
    /// STK-CTGCD X(01) : 板別註記
    /// </summary>
    public string StkCtgcd
    {
        get => this["STK-CTGCD"].Get<string>();
        set => this["STK-CTGCD"].Set(value);
    }
}
```

</details>

<br><br>

# 參數說明

## `-v`, `--verbose`
額外顯示 Copybook 解析後的資料綱要內容

```bash
forge --copybook Copybooks/t30-otc.cpy --verbose
```

<br>

<details>
    <summary>產出的 Wrapper Class 內容：</summary>

```bash
==== LAYOUT ====
COPYBOOK-LAYOUT
  01 STOCK-NO [股票代號] >> PIC: [X(6)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
  01 BULL-PRICE [漲停價] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  01 LDC-PRICE [開盤競價基準] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  01 BEAR-PRICE [跌停價] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  01 LAST-MTH-DATE [上次成交日] >> PIC: [9(8)] Class='Numeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
  01 SETTYPE [交易方式] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 MARK-W [處置股票註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 MARK-P [注意股票註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 MARK-L [委託限制註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 IND-CODE [產業別代碼] >> PIC: [X(02)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=2, Dec=0, Len=2, Usage='Display'
  01 IND-SUB-CODE [證券別代碼] >> PIC: [X(02)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=2, Dec=0, Len=2, Usage='Display'
  01 MARK-M [豁免平盤下融券賣出註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 STOCK-NAME [股票中文名稱] >> PIC: [X(16)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=16, Dec=0, Len=16, Usage='Display'
  01 MARK-W-DETAILS [處置股票資訊]
    02 MATCH-INTERVAL [撮合循環時間（分）] >> PIC: [9(03)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
    02 ORDER-LIMIT [單筆委託限制數量（張）] >> PIC: [9(06)] Class='Numeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
    02 ORDERS-LIMIT [多筆委託限制數量（張）] >> PIC: [9(06)] Class='Numeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
    02 PREPAY-RATE [款券預收成數（%）] >> PIC: [9(03)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
  01 MARK-S [豁免平盤下借券賣出註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 STK-MARK [類股註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 MARK-F [面額註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 MARK-DAY-TRADE [可現股當沖註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 STK-CTGCD [板別註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  01 FILLER >> PIC: [X(11)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=11, Dec=0, Len=11, Usage='Display'
================

==== Storage ====
COPYBOOK-STORAGE-MAP
  01 STOCK-NO start=1 len=6 end=7
  01 BULL-PRICE start=7 len=9 end=16
  01 LDC-PRICE start=16 len=9 end=25
  01 BEAR-PRICE start=25 len=9 end=34
  01 LAST-MTH-DATE start=34 len=8 end=42
  01 SETTYPE start=42 len=1 end=43
  01 MARK-W start=43 len=1 end=44
  01 MARK-P start=44 len=1 end=45
  01 MARK-L start=45 len=1 end=46
  01 IND-CODE start=46 len=2 end=48
  01 IND-SUB-CODE start=48 len=2 end=50
  01 MARK-M start=50 len=1 end=51
  01 STOCK-NAME start=51 len=16 end=67
  01 MARK-W-DETAILS start=67
    02 MATCH-INTERVAL start=67 len=3 end=70
    02 ORDER-LIMIT start=70 len=6 end=76
    02 ORDERS-LIMIT start=76 len=6 end=82
    02 PREPAY-RATE start=82 len=3 end=85
  01 MARK-S start=85 len=1 end=86
  01 STK-MARK start=86 len=1 end=87
  01 MARK-F start=87 len=1 end=88
  01 MARK-DAY-TRADE start=88 len=1 end=89
  01 STK-CTGCD start=89 len=1 end=90
  01 FILLER start=90 len=11 end=101
================

New wrapper class generated: "D:\Projects\get-the-picture\GetThePicture.Forge\T30Otc.cs"
```

</details>

<br><br>
