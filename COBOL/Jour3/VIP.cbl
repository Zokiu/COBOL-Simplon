       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIP.
       AUTHOR. Terry.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-SOLDE     PIC 9(8).
       01 WS-VIP       PIC X(9).

       PROCEDURE DIVISION.
       
           DISPLAY "Etes vous VIP ou STANDARD ?".
           ACCEPT WS-VIP.

           DISPLAY "Quel est votre solde ?".
           ACCEPT WS-SOLDE.

           EVALUATE WS-VIP
               WHEN = "VIP" AND WS-SOLDE > 10000
                   DISPLAY "Vous etes un membre premium !"
               WHEN = "VIP" AND WS-SOLDE < 10001
                   DISPLAY "Vous etes un membre privilegie"
               WHEN = "STANDARD" AND WS-SOLDE > 5000
                   DISPLAY "Vous etes un membre fidele"
               WHEN OTHER
                   DISPLAY "Vous etes un membre standard"
           END-EVALUATE.
