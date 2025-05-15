       IDENTIFICATION DIVISION.
       PROGRAM-ID. FizzBuzz.
       AUTHOR. Terry.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMB     PIC 9(3).
       01 RESULT3  PIC 9(3).
       01 WASTE3   PIC 9(3).
       01 RESULT5  PIC 9(3).
       01 WASTE5   PIC 9(3).

       PROCEDURE DIVISION.
       
           DISPLAY "Choisissez un nombre entre 1 et 100".
           ACCEPT NUMB.

           DIVIDE NUMB BY 3 GIVING RESULT3 REMAINDER WASTE3.
           DIVIDE NUMB BY 5 GIVING RESULT5 REMAINDER WASTE5.

           IF WASTE3 = 0 AND WASTE5 != 0
               THEN DISPLAY "Fizz"

           ELSE IF WASTE5 = 0 AND WASTE3 != 0
               THEN DISPLAY "Buzz"

           ELSE IF WASTE3 = 0 AND WASTE5 = 0
               THEN DISPLAY "FizzBuzz"

           ELSE 
               DISPLAY NUMB

           END-IF.
