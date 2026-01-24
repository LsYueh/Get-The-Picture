      *-------------------------------
      * Sample COBOL Copybook
      *-------------------------------
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01 CUSTOMER-RECORD.
           05 CUSTOMER-ID        PIC 9(5).
           05 CUSTOMER-NAME      PIC X(30).
           05 CUSTOMER-BALANCE   PIC S9(7)V99 COMP-3.
      *
       01 ORDER-RECORD.
           05 ORDER-ID           PIC 9(6).
           05 ORDER-DATE         PIC 9(8).
           05 ORDER-AMOUNT       PIC S9(7)V99 COMP-3.
      *
       01 LONG-DESCRIPTION.
           05 DESC-LINE          PIC X(50) VALUE
               'THIS IS A VERY LONG DESCRIPTION THAT '
      -        'NEEDS TO BE CONTINUED ACROSS MULTIPLE LINES'.
