       IDENTIFICATION DIVISION.
       PROGRAM-ID. Bisext.
      *    Demander de saisir une année et savoir 
      *    s'il elle est bisextile
      *    (divisible par 4, 400 et non 100)
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    Variable numérique 4 chiffres
       01  WS-YEAR PIC 9(4).
      *    Variable pour stocker le reste de la division
       01  WS-REST1 PIC 9(03).
       01  WS-REST2 PIC 9(03).
       01  WS-REST3 PIC 9(03).
      *    Trash
       01  WS-TRASH PIC 9.
       PROCEDURE DIVISION.
      *    Saisie de l'utilisateur avec message
           DISPLAY "Saisir une année :".
           ACCEPT WS-YEAR.
      *    Première division
           DIVIDE WS-YEAR BY 4 GIVING WS-TRASH REMAINDER WS-REST1.
      *    Deuxième division
           DIVIDE WS-YEAR BY 400 GIVING WS-TRASH REMAINDER WS-REST2.
      *    Troisième division
           DIVIDE WS-YEAR BY 100 GIVING WS-TRASH REMAINDER WS-REST3.
      *    Vérification si divisible par 100
           IF WS-REST1 = 0 AND WS-REST2 = 0
      *    Vérification si divisible par 4 et non par 100
                IF WS-REST3 NOT EQUAL 0 
      *    Test autre condition
      *    ELSE IF WS-REST1 = 0 AND WS-REST2 = 0
                   THEN DISPLAY "Non Bisextile"
                END-IF
                DISPLAY "Bisextile"
                ELSE DISPLAY "Non Bisextile"
           END-IF.
           

       STOP RUN.
