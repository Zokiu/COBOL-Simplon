       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tablo2.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-CARNET.
        05 WS-NOTE         PIC 9(02) OCCURS 5 TIMES.
        05 WS-MOYENNE      PIC 9(02)v99.
       
       01 WS-CALCUL        PIC 9(03).
       
       77 WS-INDEX         PIC 9(01).

       PROCEDURE DIVISION.
           
           PERFORM VARYING WS-INDEX
                   FROM 1 BY 1 UNTIL WS-INDEX > 5
             DISPLAY "Saisir la note"
             ACCEPT WS-NOTE(WS-INDEX)
             ADD    WS-NOTE(WS-INDEX) TO WS-CALCUL
           END-PERFORM.

           DIVIDE WS-CALCUL BY 5 GIVING WS-MOYENNE.
           DISPLAY "La moyenne de la classe est de : ".
           DISPLAY WS-MOYENNE.

           STOP RUN.
