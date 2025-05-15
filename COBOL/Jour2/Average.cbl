       IDENTIFICATION DIVISION.
       PROGRAM-ID. Average.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  NUMBER-A     PIC S9(5)v99.
       01  NUMBER-B     PIC S9(5)v99.
       01  NUMBER-C     PIC S9(5)v99.
       01  RESULT       PIC S9(5)v99.
       
       PROCEDURE DIVISION.
           

           DISPLAY "Entrez le premier chiffre".
           ACCEPT NUMBER-A.
           
           DISPLAY "Entrez le deuxième chiffre".
           ACCEPT NUMBER-B.

           DISPLAY "Entrez le troisième chiffre".
           ACCEPT NUMBER-C.
           
           MOVE FUNCTION MEAN(NUMBER-A, NUMBER-B, NUMBER-C) TO RESULT.

           DISPLAY "La moyenne des trois chiffres est :" 
           FUNCTION TRIM(RESULT).
           
           STOP RUN.
