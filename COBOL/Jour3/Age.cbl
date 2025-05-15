       IDENTIFICATION DIVISION.
       PROGRAM-ID. Age.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 AGE PIC 9(3).
       
       PROCEDURE DIVISION.
           
           DISPLAY "Quel age avez vous ?".
           ACCEPT AGE.

           EVALUATE AGE
                WHEN < 12
                    DISPLAY "Vous etes un enfant !"
                WHEN > 11 AND < 18
                    DISPLAY "Vous etes un adolescent !"
                WHEN > 17 AND < 65
                    DISPLAY "Vous etes un adulte !"
                WHEN > 65
                    DISPLAY "Vous etes un senior !"
           END-EVALUATE.
       
           STOP RUN.
           