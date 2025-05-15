       IDENTIFICATION DIVISION.
       PROGRAM-ID. Biggest.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-NUMB    PIC S9(4) VALUE  1.
       01 WS-BIGGEST PIC S9(4) VALUE -1.

       PROCEDURE DIVISION.
      *On arrête dès qu'on saisie 0
           PERFORM UNTIL WS-NUMB = 0

           DISPLAY "Choisissez un chiffre entre -999 et 999"
           ACCEPT WS-NUMB
           
      *On stocke NUMB dans BIGGEST si NUMB est plus grand sinon inchangé
           IF WS-NUMB > WS-BIGGEST
               MOVE WS-NUMB TO WS-BIGGEST

           END-PERFORM.

           DISPLAY "Votre plus grande valeure saisie est " WS-BIGGEST.

           STOP RUN.
