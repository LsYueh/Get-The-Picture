|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  ORDER-RECORD.
           05  ORDER-ID              PIC X(10).
           05  CUSTOMER-NAME         PIC X(20).

           05  ORDER-LINES           OCCURS 3 TIMES.
               10  PRODUCT-CODE      PIC X(8).
               10  QUANTITY          PIC 9(3).
               10  LINE-AMOUNTS      OCCURS 2 TIMES.
                   15  AMOUNT        PIC 9(5)V99.

           05  TOTAL-AMOUNT          PIC 9(7)V99.
