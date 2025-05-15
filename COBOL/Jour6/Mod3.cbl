       IDENTIFICATION DIVISION.
       PROGRAM-ID. Mod3.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           
       01 WS-NUMB         PIC 9(3).
       01 WS-RESULT       PIC 9(1).
       01 WS-REMAINDER    PIC 9(3).

       PROCEDURE DIVISION.
      *Test de la variable avant donc variable à 101 pour aller à 100
           PERFORM VARYING WS-NUMB
                   FROM 1 BY 1 UNTIL WS-NUMB = 101
      *On divise la valeure itérée par 3
      *On stocke le résultat dans une variable poubelle
      *On garde le reste dans une variable pour test ultérieur
           DIVIDE WS-NUMB BY 3 GIVING WS-RESULT REMAINDER WS-REMAINDER

      *On teste si le reste de la division est bien à 0
           IF WS-REMAINDER = 0
               THEN DISPLAY WS-NUMB " est un multiple de 3"

           END-PERFORM.

           STOP RUN.
