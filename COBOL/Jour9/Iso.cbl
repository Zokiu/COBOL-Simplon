      *Vérifier si un mot est un isogramme c'est à dire si chaque lettre
      * apparait une seule fois
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Iso.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-WORD.
           05 WS-WORD-LETTER  PIC X(01) OCCURS 20 TIMES.
    
       01  WS-ISO             PIC X(03) VALUE "YES".
        
       77  WS-INDEX           PIC 9(02) VALUE 1.
       77  WS-INDEX2          PIC 9(02) VALUE 1.

       PROCEDURE DIVISION.
           DISPLAY "Veuillez entrer votre mot".
           ACCEPT WS-WORD.
           MOVE FUNCTION UPPER-CASE(WS-WORD) TO WS-WORD.

           PERFORM VARYING WS-INDEX FROM 1 BY 1 
                       UNTIL WS-INDEX > 19
                       OR WS-ISO = "NO"
                       OR WS-WORD-LETTER(WS-INDEX) = SPACE
                   COMPUTE WS-INDEX2 = WS-INDEX + 1
                   PERFORM VARYING WS-INDEX2 FROM WS-INDEX2 BY 1
                       UNTIL WS-INDEX2 > 20
                       OR WS-WORD-LETTER(WS-INDEX2) = SPACE
                       IF WS-WORD-LETTER(WS-INDEX) EQUAL
                                   WS-WORD-LETTER(WS-INDEX2)
                            MOVE "NO" TO WS-ISO
                       END-IF
                   END-PERFORM
           END-PERFORM.

           IF WS-ISO = "YES"
                DISPLAY "Votre mot est un isogramme"
           ELSE
                DISPLAY "Votre mot n'est pas un isogramme"
           END-IF.
           
           STOP RUN.
