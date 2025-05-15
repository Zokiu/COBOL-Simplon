       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-SOLDE     PIC 9(8).
       01 WS-VIP       PIC X(9) VALUE UPPER-CASE.

       PROCEDURE DIVISION.

           DISPLAY "Etes vous VIP ou STANDARD ?".
           ACCEPT WS-VIP.

           DISPLAY "Quel est votre solde ?".
           ACCEPT WS-SOLDE.

           IF WS-VIP = "VIP" AND WS-SOLDE > 10000
                   THEN DISPLAY "Vous etes un membre premium !"
           ELSE IF WS-VIP = "VIP" AND WS-SOLDE < 10001
                   THEN DISPLAY "Vous etes un membre privilegie"
           ELSE IF WS-VIP = "STANDARD" AND WS-SOLDE > 5000
                   THEN DISPLAY "Vous etes un membre fidele"
           ELSE IF WS-VIP = "STANDARD" AND WS-SOLDE < 5001
                   DISPLAY "Vous etes un membre standard"
           ELSE
                   DISPLAY "Vous avez saisi : " WS-VIP
           END-IF.
