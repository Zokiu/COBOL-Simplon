       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tables.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-NUMB   PIC S9(4).
       01 WS-TABLE  PIC  9(2).
       01 WS-RESULT PIC S9(5).

       PROCEDURE DIVISION.
           
           DISPLAY "Choisissez un chiffre entre -999 et 999"
           ACCEPT WS-NUMB.

      *On itère WS-TABLE pour parcourir la table de multiplication
           PERFORM VARYING WS-TABLE
                   FROM 1 BY 1 UNTIL WS-TABLE = 11
             
      *On fais la multiplication du chiffre choisi WS-NUMB
      *Avec la valeur actuelle de WS-TABLE et donne WS-RESULT
               MULTIPLY WS-NUMB BY WS-TABLE GIVING WS-RESULT
      *Affiche le calcul
               DISPLAY WS-NUMB " x " WS-TABLE " = " WS-RESULT
           
           END-PERFORM.
           
           STOP RUN.
           