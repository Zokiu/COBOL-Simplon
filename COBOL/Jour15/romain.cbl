       IDENTIFICATION DIVISION.
       PROGRAM-ID. romain.
       AUTHOR.    Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-SAISIE    PIC 9(04).

       01  WS-ROMAIN    PIC X(20).


       PROCEDURE DIVISION.
       
           DISPLAY "Choisissez un nombre entre 1 et 3999 :"
           ACCEPT WS-SAISIE

           PERFORM UNTIL WS-SAISIE <= 0
               EVALUATE WS-SAISIE
                   WHEN >= 1000
                       STRING FUNCTION TRIM(WS-ROMAIN) "M"  
                                   INTO WS-ROMAIN
                       SUBTRACT 1000 FROM WS-SAISIE
                   WHEN >= 900
                       STRING FUNCTION TRIM(WS-ROMAIN) "CM" 
                                   INTO WS-ROMAIN
                       SUBTRACT 900  FROM WS-SAISIE
                   WHEN >= 500
                       STRING FUNCTION TRIM(WS-ROMAIN) "D"  
                                   INTO WS-ROMAIN
                       SUBTRACT 500  FROM WS-SAISIE
                   WHEN >= 400
                       STRING FUNCTION TRIM(WS-ROMAIN) "CD" 
                                   INTO WS-ROMAIN
                       SUBTRACT 400  FROM WS-SAISIE
                   WHEN >= 100
                       STRING FUNCTION TRIM(WS-ROMAIN) "C"  
                                   INTO WS-ROMAIN
                       SUBTRACT 100  FROM WS-SAISIE
                   WHEN >= 90
                       STRING FUNCTION TRIM(WS-ROMAIN) "XC" 
                                   INTO WS-ROMAIN
                       SUBTRACT 90   FROM WS-SAISIE
                   WHEN >= 50
                       STRING FUNCTION TRIM(WS-ROMAIN) "L"  
                                   INTO WS-ROMAIN
                       SUBTRACT 50   FROM WS-SAISIE
                   WHEN >= 40
                       STRING FUNCTION TRIM(WS-ROMAIN) "XL" 
                                   INTO WS-ROMAIN
                       SUBTRACT 40   FROM WS-SAISIE
                   WHEN >= 10
                       STRING FUNCTION TRIM(WS-ROMAIN) "X"  
                                   INTO WS-ROMAIN
                       SUBTRACT 10   FROM WS-SAISIE
                   WHEN >= 9
                       STRING FUNCTION TRIM(WS-ROMAIN) "IX" 
                                   INTO WS-ROMAIN
                       SUBTRACT 9    FROM WS-SAISIE
                   WHEN >= 5
                       STRING FUNCTION TRIM(WS-ROMAIN) "V"  
                                   INTO WS-ROMAIN
                       SUBTRACT 5    FROM WS-SAISIE
                   WHEN >= 4
                       STRING FUNCTION TRIM(WS-ROMAIN) "IV" 
                                   INTO WS-ROMAIN
                       SUBTRACT 4    FROM WS-SAISIE
                   WHEN OTHER
                       STRING FUNCTION TRIM(WS-ROMAIN) "I"  
                                   INTO WS-ROMAIN
                       SUBTRACT 1    FROM WS-SAISIE
               END-EVALUATE

           END-PERFORM.
           
           DISPLAY "En chiffre romain cela donne :"
           DISPLAY WS-ROMAIN.

           STOP RUN.
      
      ******************************************************************


