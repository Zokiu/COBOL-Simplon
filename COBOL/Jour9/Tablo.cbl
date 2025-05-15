       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tablo.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-CARNET.
        03 WS-ELEVE        OCCURS 3 TIMES.
         05 WS-NOM         PIC X(20).
         05 WS-NOTE        PIC 9(02).
       77  WS-NOTE-INDEX   PIC 9(01).

       PROCEDURE DIVISION.
       
           PERFORM VARYING WS-NOTE-INDEX
                   FROM 1 BY 1 UNTIL WS-NOTE-INDEX > 3
            DISPLAY "Saisir la note"
            ACCEPT  WS-NOTE(WS-NOTE-INDEX)
           END-PERFORM.
           
           DISPLAY "display Carnet".
           DISPLAY WS-CARNET.

           PERFORM VARYING WS-NOTE-INDEX 
                   FROM 1 BY 1 UNTIL WS-NOTE-INDEX > 3
            DISPLAY "display Note" WS-NOTE-INDEX
            DISPLAY WS-NOTE(WS-NOTE-INDEX)
           END-PERFORM.

           STOP RUN.
