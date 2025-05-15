       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tablo3.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-CARNET.
        05  WS-ELEVE     OCCURS 3 TIMES.
         10  WS-MATIERE  OCCURS 4 TIMES.
           15 WS-NOTE    PIC 9(02).

       77  WS-INDEX-1   PIC 9.
       77  WS-INDEX-2   PIC 9.

       PROCEDURE DIVISION.
       
           PERFORM VARYING   WS-INDEX-1
                       FROM 1 BY 1 UNTIL WS-INDEX-1 > 3
             PERFORM VARYING WS-INDEX-2
                       FROM 1 BY 1 UNTIL WS-INDEX-2 > 4
                DISPLAY "Saisir la note " WITH NO ADVANCING
                DISPLAY "de la matière N°" WS-INDEX-2
                DISPLAY "Pour l'élève N°" WS-INDEX-1
                ACCEPT  WS-NOTE(WS-INDEX-1, WS-INDEX-2)
             END-PERFORM
           END-PERFORM.
           
           PERFORM VARYING   WS-INDEX-1
                       FROM 1 BY 1 UNTIL WS-INDEX-1 > 3
             PERFORM VARYING WS-INDEX-2
                       FROM 1 BY 1 UNTIL WS-INDEX-2 > 4
                DISPLAY "La note " WITH NO ADVANCING
                DISPLAY "de la matière N°" WS-INDEX-2
                DISPLAY "Pour l'élève N°" WS-INDEX-1
                DISPLAY WS-NOTE(WS-INDEX-1, WS-INDEX-2)
             END-PERFORM
           END-PERFORM.

           STOP RUN.
