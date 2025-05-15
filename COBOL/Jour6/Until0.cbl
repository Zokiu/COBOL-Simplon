       IDENTIFICATION DIVISION.
       PROGRAM-ID. Until0.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-TEST PIC 9(5) VALUE 1.

       PROCEDURE DIVISION.
      
      *On effectue la série d'instruction jusque var = 0
           PERFORM UNTIL WS-TEST = 0
           
           DISPLAY "Choisissez un chiffre :"
           ACCEPT     WS-TEST
           
           DISPLAY "Vous avez choisi :"
           DISPLAY    WS-TEST

           END-PERFORM.
           
           STOP RUN.
           
       