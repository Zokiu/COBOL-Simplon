       IDENTIFICATION DIVISION.
       PROGRAM-ID. Signe.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUMBER-A PIC S9(4)v99.
       
       PROCEDURE DIVISION.
           
       DISPLAY "Choisissez un nombre".
       ACCEPT NUMBER-A.

       IF NUMBER-A < 0
           THEN DISPLAY "Votre nombre est negatif"
       ELSE 
           DISPLAY "Votre nombre est positif"
       END-IF.

           STOP RUN.
