|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  T30-OTC.
           05  T30-KEY.
               10  STOCK-NO        PIC X(6).       *> 股票代號
           05  BULL-PRICE          PIC 9(5)V9(4).  *> 漲停價
           05  LDC-PRICE           PIC 9(5)V9(4).  *> 開盤競價基準
           05  BEAR-PRICE          PIC 9(5)V9(4).  *> 跌停價
           05  LAST-MTH-DATE       PIC 9(8).       *> 上次成交日
           05  SETTYPE             PIC X(01).      *> 交易方式
           05  MARK-W              PIC X(01).      *> 處置股票註記
           05  MARK-P              PIC X(01).      *> 注意股票註記
           05  MARK-L              PIC X(01).      *> 委託限制註記
           05  IND-CODE            PIC X(02).      *> 產業別代碼
           05  IND-SUB-CODE        PIC X(02).      *> 證券別代碼
           05  MARK-M              PIC X(01).      *> 豁免平盤下融券賣出註記
           05  STOCK-NAME          PIC X(16).      *> 股票中文名稱
           05  MARK-W-DETAILS.                     *> 處置股票資訊
               10 MATCH-INTERVAL   PIC 9(03).      *> 撮合循環時間（分）
               10 ORDER-LIMIT      PIC 9(06).      *> 單筆委託限制數量（張）
               10 ORDERS-LIMIT     PIC 9(06).      *> 多筆委託限制數量（張）
               10 PREPAY-RATE      PIC 9(03).      *> 款券預收成數（%）
           05  MARK-S              PIC X(01).      *> 豁免平盤下借券賣出註記
           05  STK-MARK            PIC X(01).      *> 類股註記
           05  MARK-F              PIC X(01).      *> 面額註記
           05  MARK-DAY-TRADE      PIC X(01).      *> 可現股當沖註記
           05  STK-CTGCD           PIC X(01).      *> 板別註記
           05  FILLER              PIC X(11).