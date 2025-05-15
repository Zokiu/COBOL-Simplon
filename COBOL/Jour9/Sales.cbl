       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sales.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-SALES.
        05  WS-PRODUCT        OCCURS 4 TIMES.
         10  WS-PRODUCT-NAME  PIC X(20).
         10  WS-PRODUCT-SALES PIC 9(03).
         10  WS-PRODUCT-PRICE PIC 9(02)v99.
         10  WS-PRODUCT-TOTAL PIC 9(04)v99.
       
       
       77  WS-INDEX           PIC 9(01).

       PROCEDURE DIVISION.
       
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL   WS-INDEX > 5
           ACCEPT WS-PRODUCT-SALES(WS-INDEX)
           DISPLAY WS-PRODUCT-NAME(WS-INDEX) " a été vendu " 
                  WS-PRODUCT-SALES(WS-INDEX)
                 " fois cette semaine au prix de " 
                  WS-PRODUCT-PRICE(WS-INDEX)
           MULTIPLY WS-PRODUCT-SALES(WS-INDEX)
                 BY WS-PRODUCT-PRICE(WS-INDEX)
             GIVING WS-PRODUCT-TOTAL(WS-INDEX)
           MOVE   WS-PRODUCT-TOTAL(WS-INDEX) 
               INTO WS-WEEK-NUMBER(WS-INDEX)
           
           END-PERFORM.

           DISPLAY "La valeur total du stock vendu cette semaine est: ".
           DISPLAY 

           STOP RUN.
