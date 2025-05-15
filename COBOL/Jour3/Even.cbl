       IDENTIFICATION DIVISION.
       PROGRAM-ID. Even.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMB       PIC S9(4).
       01 RESULT     PIC S9(4).
       01 WASTE      PIC 9(4).

       PROCEDURE DIVISION.
       
           DISPLAY "Choisissez un nombre entier".
           ACCEPT NUMB.
           DIVIDE NUMB BY 2 GIVING RESULT REMAINDER WASTE.

           IF WASTE = 0
               THEN DISPLAY "Votre nombre est pair"
           ELSE 
               DISPLAY "Votre nombre est impair"
           END-IF.

           STOP RUN.

       