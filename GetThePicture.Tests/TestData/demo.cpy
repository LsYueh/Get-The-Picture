      * Sample COBOL Copybook
      * Defines a fixed-length record layout
      * Total length: 23 bytes
|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID       PIC 9(8).
           05  CUSTOMER-NAME     PIC X(10).
           05  ACCOUNT-BALANCE   PIC S9(5)V99  COMP-3.
       