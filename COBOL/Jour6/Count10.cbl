       IDENTIFICATION DIVISION.
       PROGRAM-ID. Count10.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-NUMB PIC 99.

       PROCEDURE DIVISION.
      *Test de la variable avant donc variable à 11 pour afficher 10.
           PERFORM VARYING WS-NUMB
               FROM 1 BY 1 UNTIL WS-NUMB = 11

               DISPLAY WS-NUMB

           END-PERFORM.

           STOP RUN.

       