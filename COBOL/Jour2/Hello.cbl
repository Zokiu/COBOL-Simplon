       IDENTIFICATION DIVISION.
       PROGRAM-ID. Hello.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 HELLO PIC X(5) VALUE "Salut".
       01 NAMES PIC X(5).
       
       PROCEDURE DIVISION.
           
           DISPLAY "Quel est votre nom ?".

           ACCEPT NAMES.

           DISPLAY HELLO " " NAMES.

           STOP RUN.
