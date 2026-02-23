|...+.*..1....+....2....+....3....+....4....+....5....+....6....+....7..
       01 ORDER-TABLE.
           05 ORDER-STATUS PIC X OCCURS 5 TIMES.
               88 ORDER-SHIPPED   VALUE 'S'.
               88 ORDER-PENDING   VALUE 'P'.
               88 ORDER-CANCELLED VALUE 'C'.