# Forge
將 `Copybook` 中的 PICTURE / 資料欄位結構，轉換成 `CbWarpper` 的子類別。

<br>

# 使用方式

使用 `--copybook` 指定要轉換的 Copybook  
```bash
forge --copybook twse/t30-otc.cpy
```

輸出結果:  
```bash
New warpper class generated: "D:\Projects\get-the-picture\GetThePicture.Tests\TestData\T30Otc.cs"
```

<br>

<details>
    <summary>產出的 Warpper Class 內容：</summary>

```csharp
using GetThePicture.Copybook.Warpper;
using GetThePicture.Copybook.Warpper.Base;

namespace GetThePicture;

public class T30Otc_t(byte[] raw) : CbWarpper(raw)
{
    // ----------------------------
    // Copybook Address Map
    // ----------------------------

    protected override Dictionary<string, CbAddress> AddressMap { get; } = new Dictionary<string, CbAddress>
    {
        ["STOCK-NO"]                       = new CbAddress(   1,   6, "X(6)"),
        ["BULL-PRICE"]                     = new CbAddress(   7,   9, "9(5)V9(4)"),
        ["LDC-PRICE"]                      = new CbAddress(  16,   9, "9(5)V9(4)"),
        ["BEAR-PRICE"]                     = new CbAddress(  25,   9, "9(5)V9(4)"),
        ["LAST-MTH-DATE"]                  = new CbAddress(  34,   8, "9(8)"),
        ["SETTYPE"]                        = new CbAddress(  42,   1, "X(01)"),
        ["MARK-W"]                         = new CbAddress(  43,   1, "X(01)"),
        ["MARK-P"]                         = new CbAddress(  44,   1, "X(01)"),
        ["MARK-L"]                         = new CbAddress(  45,   1, "X(01)"),
        ["IND-CODE"]                       = new CbAddress(  46,   2, "X(02)"),
        ["IND-SUB-CODE"]                   = new CbAddress(  48,   2, "X(02)"),
        ["MARK-M"]                         = new CbAddress(  50,   1, "X(01)"),
        ["STOCK-NAME"]                     = new CbAddress(  51,  16, "X(16)"),
        ["MARK-W-DETAILS::MATCH-INTERVAL"] = new CbAddress(  67,   3, "9(03)"),
        ["MARK-W-DETAILS::ORDER-LIMIT"]    = new CbAddress(  70,   6, "9(06)"),
        ["MARK-W-DETAILS::ORDERS-LIMIT"]   = new CbAddress(  76,   6, "9(06)"),
        ["MARK-W-DETAILS::PREPAY-RATE"]    = new CbAddress(  82,   3, "9(03)"),
        ["MARK-S"]                         = new CbAddress(  85,   1, "X(01)"),
        ["STK-MARK"]                       = new CbAddress(  86,   1, "X(01)"),
        ["MARK-F"]                         = new CbAddress(  87,   1, "X(01)"),
        ["MARK-DAY-TRADE"]                 = new CbAddress(  88,   1, "X(01)"),
        ["STK-CTGCD"]                      = new CbAddress(  89,   1, "X(01)"),
        ["FILLER01"]                       = new CbAddress(  90,  11, "X(11)"),
    };

    // ----------------------------
    // Strongly Typed Properties
    // ----------------------------

    public string StockNo
    {
        get => (string)this["STOCK-NO"]!;
        set => this["STOCK-NO"] = value;
    }

    public uint BullPrice
    {
        get => (uint)this["BULL-PRICE"]!;
        set => this["BULL-PRICE"] = value;
    }

    public uint LdcPrice
    {
        get => (uint)this["LDC-PRICE"]!;
        set => this["LDC-PRICE"] = value;
    }

    public uint BearPrice
    {
        get => (uint)this["BEAR-PRICE"]!;
        set => this["BEAR-PRICE"] = value;
    }

    public uint LastMthDate
    {
        get => (uint)this["LAST-MTH-DATE"]!;
        set => this["LAST-MTH-DATE"] = value;
    }

    public string Settype
    {
        get => (string)this["SETTYPE"]!;
        set => this["SETTYPE"] = value;
    }

    public string MarkW
    {
        get => (string)this["MARK-W"]!;
        set => this["MARK-W"] = value;
    }

    public string MarkP
    {
        get => (string)this["MARK-P"]!;
        set => this["MARK-P"] = value;
    }

    public string MarkL
    {
        get => (string)this["MARK-L"]!;
        set => this["MARK-L"] = value;
    }

    public string IndCode
    {
        get => (string)this["IND-CODE"]!;
        set => this["IND-CODE"] = value;
    }

    public string IndSubCode
    {
        get => (string)this["IND-SUB-CODE"]!;
        set => this["IND-SUB-CODE"] = value;
    }

    public string MarkM
    {
        get => (string)this["MARK-M"]!;
        set => this["MARK-M"] = value;
    }

    public string StockName
    {
        get => (string)this["STOCK-NAME"]!;
        set => this["STOCK-NAME"] = value;
    }

    public ushort MarkWDetails_MatchInterval
    {
        get => (ushort)this["MARK-W-DETAILS::MATCH-INTERVAL"]!;
        set => this["MARK-W-DETAILS::MATCH-INTERVAL"] = value;
    }

    public uint MarkWDetails_OrderLimit
    {
        get => (uint)this["MARK-W-DETAILS::ORDER-LIMIT"]!;
        set => this["MARK-W-DETAILS::ORDER-LIMIT"] = value;
    }

    public uint MarkWDetails_OrdersLimit
    {
        get => (uint)this["MARK-W-DETAILS::ORDERS-LIMIT"]!;
        set => this["MARK-W-DETAILS::ORDERS-LIMIT"] = value;
    }

    public ushort MarkWDetails_PrepayRate
    {
        get => (ushort)this["MARK-W-DETAILS::PREPAY-RATE"]!;
        set => this["MARK-W-DETAILS::PREPAY-RATE"] = value;
    }

    public string MarkS
    {
        get => (string)this["MARK-S"]!;
        set => this["MARK-S"] = value;
    }

    public string StkMark
    {
        get => (string)this["STK-MARK"]!;
        set => this["STK-MARK"] = value;
    }

    public string MarkF
    {
        get => (string)this["MARK-F"]!;
        set => this["MARK-F"] = value;
    }

    public string MarkDayTrade
    {
        get => (string)this["MARK-DAY-TRADE"]!;
        set => this["MARK-DAY-TRADE"] = value;
    }

    public string StkCtgcd
    {
        get => (string)this["STK-CTGCD"]!;
        set => this["STK-CTGCD"] = value;
    }
}

```

</details>

<br><br>

# 參數說明

## `-v`, `--verbose`
額外顯示 Copybook 解析後的資料綱要內容

```bash
forge --copybook twse/t30-otc.cpy --verbose
```

<br>

<details>
    <summary>產出的 Warpper Class 內容：</summary>

```bash
==== LAYOUT ====
COPYBOOK-LAYOUT
  1 STOCK-NO [股票代號] >> PIC: [X(6)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
  1 BULL-PRICE [漲停價] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  1 LDC-PRICE [開盤競價基準] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  1 BEAR-PRICE [跌停價] >> PIC: [9(5)V9(4)] Class='Numeric' (Semantic='None'), Signed=False, Int=5, Dec=4, Len=9, Usage='Display'
  1 LAST-MTH-DATE [上次成交日] >> PIC: [9(8)] Class='Numeric' (Semantic='None'), Signed=False, Int=8, Dec=0, Len=8, Usage='Display'
  1 SETTYPE [交易方式] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 MARK-W [處置股票註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 MARK-P [注意股票註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 MARK-L [委託限制註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 IND-CODE [產業別代碼] >> PIC: [X(02)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=2, Dec=0, Len=2, Usage='Display'
  1 IND-SUB-CODE [證券別代碼] >> PIC: [X(02)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=2, Dec=0, Len=2, Usage='Display'
  1 MARK-M [豁免平盤下融券賣出註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 STOCK-NAME [股票中文名稱] >> PIC: [X(16)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=16, Dec=0, Len=16, Usage='Display'
  1 MARK-W-DETAILS [處置股票資訊]
    2 MATCH-INTERVAL [撮合循環時間（分）] >> PIC: [9(03)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
    2 ORDER-LIMIT [單筆委託限制數量（張）] >> PIC: [9(06)] Class='Numeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
    2 ORDERS-LIMIT [多筆委託限制數量（張）] >> PIC: [9(06)] Class='Numeric' (Semantic='None'), Signed=False, Int=6, Dec=0, Len=6, Usage='Display'
    2 PREPAY-RATE [款券預收成數（%）] >> PIC: [9(03)] Class='Numeric' (Semantic='None'), Signed=False, Int=3, Dec=0, Len=3, Usage='Display'
  1 MARK-S [豁免平盤下借券賣出註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 STK-MARK [類股註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 MARK-F [面額註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 MARK-DAY-TRADE [可現股當沖註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 STK-CTGCD [板別註記] >> PIC: [X(01)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=1, Dec=0, Len=1, Usage='Display'
  1 FILLER >> PIC: [X(11)] Class='Alphanumeric' (Semantic='None'), Signed=False, Int=11, Dec=0, Len=11, Usage='Display'
================

==== Storage ====
COPYBOOK-STORAGE-MAP
  STOCK-NO start=1 len=6 end=7
  BULL-PRICE start=7 len=9 end=16
  LDC-PRICE start=16 len=9 end=25
  BEAR-PRICE start=25 len=9 end=34
  LAST-MTH-DATE start=34 len=8 end=42
  SETTYPE start=42 len=1 end=43
  MARK-W start=43 len=1 end=44
  MARK-P start=44 len=1 end=45
  MARK-L start=45 len=1 end=46
  IND-CODE start=46 len=2 end=48
  IND-SUB-CODE start=48 len=2 end=50
  MARK-M start=50 len=1 end=51
  STOCK-NAME start=51 len=16 end=67
  MARK-W-DETAILS start=67
    MATCH-INTERVAL start=67 len=3 end=70
    ORDER-LIMIT start=70 len=6 end=76
    ORDERS-LIMIT start=76 len=6 end=82
    PREPAY-RATE start=82 len=3 end=85
  MARK-S start=85 len=1 end=86
  STK-MARK start=86 len=1 end=87
  MARK-F start=87 len=1 end=88
  MARK-DAY-TRADE start=88 len=1 end=89
  STK-CTGCD start=89 len=1 end=90
  FILLER start=90 len=11 end=101
================

New warpper class generated: "D:\Projects\get-the-picture\GetThePicture.Tests\TestData\T30Otc.cs"
```

</details>

<br><br>
