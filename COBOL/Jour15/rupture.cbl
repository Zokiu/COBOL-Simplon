       IDENTIFICATION DIVISION.
       PROGRAM-ID. rupture.
       AUTHOR.    Terry.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FICHIER-INVENTAIRE ASSIGN TO "inventaire.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT FICHIER-RUPTURE ASSIGN TO "rupture.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
        
       DATA DIVISION.
       FILE SECTION.

       FD FICHIER-INVENTAIRE.
       01  F-INVENTAIRE.
           05 F-ARTICLE         PIC X(10).
           05 F-STOCK           PIC 9(02).

       FD FICHIER-RUPTURE.
       01  F-RUPTURE.
           05 F-ARTICLE-RUPT    PIC X(10).
           05 F-STOCK-RUPT      PIC 9(02).

       WORKING-STORAGE SECTION.
       
       01  WS-TABLE-INVENTAIRE.
           05 WS-INVENTAIRE OCCURS 15 TIMES.
               10 WS-ARTICLE    PIC X(10).
               10 WS-STOCK      PIC 9(02).


       77  WS-INDEX             PIC 9(02) VALUE 1.
       77  WS-MAX-TABLE         PIC 9(02) VALUE 15.

       77  WS-FIN-FICHIER       PIC X     VALUE "N".

       PROCEDURE DIVISION.
       
           PERFORM 0100-READ-START
           THRU    0100-READ-END.

           PERFORM 0200-OUTPUT-START
           THRU    0200-OUTPUT-END.

           STOP RUN.
      ******************************************************************
       
       0100-READ-START.
           
           OPEN INPUT FICHIER-INVENTAIRE.

           PERFORM UNTIL WS-FIN-FICHIER = "Y"
           
               READ FICHIER-INVENTAIRE
                   AT END
                       MOVE "Y" TO WS-FIN-FICHIER
                   NOT AT END
                       IF WS-INDEX <= WS-MAX-TABLE
                           MOVE F-ARTICLE TO WS-ARTICLE(WS-INDEX)
                           MOVE F-STOCK   TO WS-STOCK(WS-INDEX)
                           ADD 1 TO WS-INDEX
                       END-IF
               END-READ
           END-PERFORM.

           CLOSE FICHIER-INVENTAIRE.

           EXIT.
       0100-READ-END.

       0200-OUTPUT-START.
           
           OPEN OUTPUT FICHIER-RUPTURE.
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-MAX-TABLE
                   IF FUNCTION NUMVAL(WS-STOCK(WS-INDEX)) = 0
                       MOVE WS-ARTICLE(WS-INDEX) TO F-ARTICLE-RUPT
                       MOVE WS-STOCK(WS-INDEX)   TO F-STOCK-RUPT
                       WRITE F-RUPTURE
                   END-IF
           END-PERFORM.

           CLOSE FICHIER-RUPTURE.

           EXIT.
       0200-OUTPUT-END.

