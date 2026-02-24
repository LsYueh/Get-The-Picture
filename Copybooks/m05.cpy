      * 檔案長度：154
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7.. 
       01  M05. *> 實物申購/買回清單資料檔
           05 PD-ID        PIC X(04).  *> 證商代號
           05 PUBLISH-DATE PIC 9(08).  *> 處理日  
           05 ETF-ID       PIC X(06).  *> 基金代號
           05 PUBLISH-TIME PIC 9(06).  *> 時間    
           05 FIELD-NAME   PIC X(04).  *> 欄位名稱
           05 FIELD-DATA   PIC X(126). *> (資料區)
           
           05 COMT-DATA REDEFINES FIELD-DATA.  *> FIELD-NAME = "COMT"
               07 COMT-VALUE   PIC X(126).     *> 中文說明資料

           05 CMEN-DATA REDEFINES FIELD-DATA.  *> FIELD-NAME = "CMEN"
               07 CMEN-VALUE   PIC X(126).     *> 英文說明資料

           05 ANCE-DATA REDEFINES FIELD-DATA.  *> FIELD-NAME = "ANCE"
               07 ANNOUNCE-YMD     PIC 9(08).  *> 公告日
               07 FILLER           PIC X(01).
               07 TOTAL-AV         PIC 9(18).  *> 總淨值
               07 FILLER           PIC X(01).
               07 NAV              PIC 9(5)V9(4). *> 單位淨值
               07 FILLER           PIC X(01).
               07 BASE-VALUE       PIC 9(08).  *> 基本單位數
               07 FILLER           PIC X(01).
               07 TOTAL-ISSUES     PIC 9(13).  *> 發行單位數
               07 FILLER           PIC X(01).
               07 ISSUES-DIFF-S    PIC X(01).  *> 交易差異數 S9(09)
               07 ISSUES-DIFF-9    PIC 9(09).  *> 交易差異數 S9(09)
               07 FILLER           PIC X(01).
               07 ESTC-VALUE       PIC 9(18).  *> 約當市值
               07 FILLER           PIC X(01).
               07 ESTD-VALUE       PIC 9(18).  *> 估計現金差額
               07 FILLER           PIC X(01).
               07 TOTAL-ISSUES-T-1 PIC 9(13).  *> T-1日發行單位數
               07 FILLER           PIC X(03).

           05 OBJ-DATA REDEFINES FIELD-DATA.   *> FIELD-NAME = "OBJ "
               07 OBJ-ID           PIC X(06).  *> 股票代號(成分股)
               07 OBJ-STOCK-NOS    PIC 9(08).  *> 股數
               07 OBJ-NOS-DIFF-S   PIC X(01). *> 與前日股數差異數 S9(07)
               07 OBJ-NOS-DIFF-9   PIC 9(07). *> 與前日股數差異數 S9(07)
               07 OBJ-PRICE        PIC 9(5)V9(4). *> 收盤價
               07 OBJ-LIEU-MARK    PIC X(01).  *> 現金替代
               07 OBJ-SUSPEND      PIC X(01).  *> 暫停交易
               07 FILLER           PIC X(93).

           05 CTRL-DATA REDEFINES FIELD-DATA.  *> FIELD-NAME = "CTRL"
               07 CREATION-S       PIC X(01).  *> 實物申購Y/N
               07 FILLER           PIC X(01).
               07 REDEMPTION-S     PIC X(01).  *> 實物贖回Y/N
               07 FILLER           PIC X(01).
               07 CREATION-C       PIC X(01).  *> 現金申購Y/N
               07 FILLER           PIC X(01).
               07 REDEMPTION-C     PIC X(01).  *> 現金贖回Y/N
               07 FILLER           PIC X(01).
               07 BASKET-VALUE     PIC 9(14).  *> 每預收申購款
               07 FILLER           PIC X(01).
               07 MAX-ISSUES       PIC 9(13).  *> 核准發行單位數
               07 FILLER           PIC X(01).
               07 BASKET-VALUE-P   PIC 9(14).  *> 前一日每實際申購總價金
               07 FILLER           PIC X(01).
               07 DIFF-BASKET-VALUE-S PIC X(01). *> 前一日申購總價金差異額 S9(09)
               07 DIFF-BASKET-VALUE-9 PIC 9(14). *> 前一日申購總價金差異額 S9(09)
               07 FILLER           PIC X(59).
