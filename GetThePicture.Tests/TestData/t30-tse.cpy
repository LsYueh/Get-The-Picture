|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  STOCK-NO            PIC X(6).       *> 股票代號
       01  BULL-PRICE          PIC 9(5)V9(4).  *> 漲停價
       01  LDC-PRICE           PIC 9(5)V9(4).  *> 開盤競價基準
       01  BEAR-PRICE          PIC 9(5)V9(4).  *> 跌停價
       01  LAST-MTH-DATE       PIC 9(8).       *> 上次成交日
       01  SETTYPE             PIC X(01).      *> 交易方式
       01  MARK-W              PIC X(01).      *> 處置股票註記
       01  MARK-P              PIC X(01).      *> 注意股票註記
       01  MARK-L              PIC X(01).      *> 委託限制註記
       01  IND-CODE            PIC X(02).      *> 產業別代碼
       01  STK-CODE            PIC X(02).      *> 證券別代碼
       01  MARK-M              PIC X(01).      *> 豁免平盤下融券賣出註記
       01  STOCK-NAME          PIC X(16).      *> 股票中文名稱
       01  MARK-W-DETAILS.                     *> 處置股票資訊
           02 MATCH-INTERVAL   PIC 9(03).      *> 撮合循環時間（分）
           02 ORDER-LIMIT      PIC 9(06).      *> 單筆委託限制數量
           02 ORDERS-LIMIT     PIC 9(06).      *> 多筆委託限制數量
           02 PREPAY-RATE      PIC 9(03).      *> 款券預收成數（%）
       01  MARK-S              PIC X(01).      *> 豁免平盤下借券賣出註記
       01  MARK-F              PIC X(01).      *> 面額註記
       01  MARK-DAY-TRADE      PIC X(01).      *> 可現股當沖註記
       01  STK-CTGCD           PIC X(01).      *> 板別註記
       01  FILLER              PIC X(12).