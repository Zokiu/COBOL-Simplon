       IDENTIFICATION DIVISION.
       PROGRAM-ID. Additio.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  NUMBER-A     PIC 9(5).
       01  NUMBER-B     PIC 9(5).
       01  RESULT       PIC 9(5).
       
       PROCEDURE DIVISION.
           

           DISPLAY "Entrez le premier chiffre".
           ACCEPT NUMBER-A.
           
           DISPLAY "Entrez le deuxième chiffre".
           ACCEPT NUMBER-B.
           
           DISPLAY "Le résultat de votre addition est :".
           ADD NUMBER-A NUMBER-B GIVING RESULT.
           
           DISPLAY RESULT.

           STOP RUN.
